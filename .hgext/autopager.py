"""Pages output using PAGER only if the output would exceed the terminal's
height.

On Unix, this extension uses ioctl(2) to determine the terminal's dimensions.
On Windows, GetConsoleScreenBufferInfo is used. If neither is available or
unable to determine the dimensions, the LINES and COLUMNS environment
variables are used.

Default settings:

    [autopager]
    ; Like in the pager extension, this helps alleviate broken pipes
    quiet = False
    ; The size of your shell prompt in lines
    promptsize = 1
    ; The pager to use
    pager = [environment variable PAGER]

If neither pager nor PAGER is set, the extension does nothing.

Note: Output to stderr is sent to the pager as well. If the pager isn't
invoked, it's preserved (instead of being sent to stdout).

Tips for using this extension with less:

* If you don't like how less clears the screen when you quit, try the
  -X/--no-init switch. This could cause other issues depending on your
  terminal emulator - or it could work fine.

* If you're using another extension that colorizes a command's output, try
  the -R/--RAW-CONTROL-CHARS switch. This will cause less to avoid escaping
  color control codes.
"""
# FIXME: Find a way to work around http://bugs.python.org/issue3242.
#        stdout needs to get reassigned at a different point (or maybe not
#        at all?)

import atexit
import os
import signal
import sys
import unicodedata

from mercurial import util

def _ioctl_dimensions(fd):
    from fcntl import ioctl
    from struct import pack, unpack
    from termios import TIOCGWINSZ
    return unpack('HHHH', ioctl(fd, TIOCGWINSZ, pack('HHHH', 0, 0, 0, 0)))[:2]


# TODO: Actually test this.
def _win_dimensions(fd):
    from ctypes import windll, create_string_buffer
    handle = windll.kernel32.GetStdHandle(fd)
    buf = create_string_buffer(22)
    res = windll.kernel32.GetConsoleScreenBufferInfo(handle, buf)
    if res:
        from struct import unpack
        left, top, right, bottom = unpack('hhhhHhhhhhh', buf.raw)[5:9]
        return (bottom - top + 1, right - left + 1)


def get_dimensions():
    """Returns terminal height and width"""

    height = width = 0
    try:
        height, width = _ioctl_dimensions(0)
        if 0 in (height, width):
            height, width = _ioctl_dimensions(1)
        if 0 in (height, width):
            height, width = _ioctl_dimensions(2)
    except ImportError:
        try:
            height, width = _win_dimensions(0)
            if 0 in (height, width):
                height, width = _win_dimensions(1)
            if 0 in (height, width):
                height, width = _win_dimensions(2)
        except ImportError:
            pass
    if not height:
        try:
            height = int(os.environ.get('LINES', 0))
        except ValueError:
            pass
    if not width:
        try:
            width = int(os.environ.get('COLUMNS', 0))
        except ValueError:
            pass
    return height, width


def wrap_output(pager, height, width):
    """Wraps stdout/stderr and sends output to pager if the number of lines
    written exceeds the terminal dimensions, otherwise the buffer is flushed
    with atexit.
    """

    buf = [[]]
    line_count = [0]
    col_count = [0]

    def flush(exiting=True):
        # This accounts for output that doesn't end with a newline whose last
        # line is wider than the terminal width (which would increase the line
        # count by one).
        if (exiting and line_count[0] >= height and col_count >= width
            and buf[0]):
            sys.stdout = sys.stderr = os.popen(pager, 'wb')
        elif hasattr(sys.stdout, '_stream'):
            sys.stdout = sys.stdout._stream
            sys.stderr = sys.stderr._stream
        for is_stderr, s in buf[0]:
            if is_stderr:
                sys.stderr.write(s)
            else:
                sys.stdout.write(s)
        buf[0] = []
    atexit.register(flush)

    class FileProxy(object):
        def __init__(self, stream, is_stderr=False):
            self._stream = stream
            self._is_stderr = is_stderr
        def __getattr__(self, name):
            return getattr(self._stream, name)
        def write(self, s):
            # The stream is recorded to preserve stdout/stderr if output
            # isn't paged.
            buf[0].append((self._is_stderr, s))
            # Try to determine the width of the string as it would appear in
            # the terminal.
            try:
                s = s.decode(util._encoding, util._encodingmode)
                s = unicodedata.normalize('NFC', s)
            except UnicodeError:
                pass
            for c in s:
                col_count[0] += 1
                if c == '\n' or col_count[0] > width:
                    line_count[0] += 1
                    col_count[0] = 0
            if line_count[0] > height:
                sys.stdout = sys.stderr = os.popen(pager, 'wb')
                flush(exiting=False)

    sys.stdout = FileProxy(sys.stdout)
    sys.stderr = FileProxy(sys.stderr, True)


def uisetup(ui, *args, **kwargs):

    pager = ui.config('autopager', 'pager', os.environ.get('PAGER'))
    if pager and sys.stdout.isatty() and '--debugger' not in sys.argv:
        height, width = get_dimensions()
        if height > 0 and width > 0:
            if ui.configbool('autopager', 'quiet'):
                signal.signal(signal.SIGPIPE, signal.SIG_DFL)
            promptsize = int(ui.config('autopager', 'promptsize', 1))
            wrap_output(pager, height - promptsize, width)

"""
def testpager(ui, *args, **kwargs):
    for i in xrange(24):
        print '-' * 80

cmdtable = {'testpager': (testpager, [], "")}

if __name__ == '__main__':
    class ui(object):
        def config(self, section, name, default=None):
            return default
        def configbool(self, section, name):
            return False
    uisetup(ui())
    testpager(None)
"""
