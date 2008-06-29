"""Pages output using PAGER only if the output would exceed the terminal's
height.

On Unix, this extension uses ioctl(2) to determine the terminal's height. On
Windows, GetConsoleScreenBufferInfo is used. If neither is available or
unable to determine the height, the LINES environment variable is used.

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
"""

import atexit
import os
import signal
import sys

def _ioctl_height(fd):
    from fcntl import ioctl
    from struct import pack, unpack
    from termios import TIOCGWINSZ
    return unpack('HHHH', ioctl(fd, TIOCGWINSZ, pack('HHHH', 0, 0, 0, 0)))[0]


# TODO: Actually test this.
def _win_height(fd):
    from ctypes import windll, create_string_buffer
    handle = windll.kernel32.GetStdHandle(fd)
    buf = create_string_buffer(22)
    res = windll.kernel32.GetConsoleScreenBufferInfo(handle, buf)
    if res:
        from struct import unpack
        left, top, right, bottom = unpack('hhhhHhhhhhh', buf.raw)[5:9]
        return bottom - top + 1


def get_height():
    """Returns terminal height"""

    height = 0
    try:
        height = _ioctl_height(0) or _ioctl_height(1) or _ioctl_height(2)
    except ImportError:
        try:
            height = _win_height(-10) or _win_height(-11) or _win_height(-12)
        except ImportError:
            pass
    if not height:
        import os
        height = os.environ.get('LINES', 0)
    return height


def wrap_output(max_lines, pager):
    """Wraps stdout/stderr and sends output to pager if the number of lines
    written exceeds max_lines, otherwise the buffer is flushed with atexit.
    """

    buf = [[]]
    def flush():
        if hasattr(sys.stdout, '_stream'):
            sys.stdout = sys.stdout._stream
            sys.stderr = sys.stderr._stream
        for is_stderr, s in buf[0]:
            if is_stderr:
                sys.stderr.write(s)
            else:
                sys.stdout.write(s)
        buf[0] = []
    atexit.register(flush)

    line_count = [0]
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
            line_count[0] += s.count('\n')
            if line_count[0] > max_lines:
                sys.stdout = sys.stderr = os.popen(pager, 'wb')
                flush()

    sys.stdout = FileProxy(sys.stdout)
    sys.stderr = FileProxy(sys.stderr, True)


def uisetup(ui, *args, **kwargs):

    pager = ui.config('autopager', 'pager', os.environ.get('PAGER'))
    if pager and sys.stdout.isatty() and '--debugger' not in sys.argv:
        max_lines = get_height()
        if max_lines > 0:
            if ui.configbool('autopager', 'quiet'):
                signal.signal(signal.SIGPIPE, signal.SIG_DFL)
            promptsize = int(ui.config('autopager', 'promptsize', 1))
            wrap_output(max_lines - promptsize, pager)
