""""Startup script that adds niceties to the interactive interpreter.

This script adds the following things:

- Readline bindings, tab completion, and history (in ~/.pyhistory,
  which can be disabled by setting NOHIST in the environment)

- Pretty printing of expression output (with Pygments highlighting)

- Pygments highlighting of tracebacks

- Function arguments in repr() for callables

- A source() function that displays the source of an arbitrary object
  (in a pager, with Pygments highlighting)

Python 2.3 and newer are supported, including Python 3.x.

Note: The default versions of Python that ship with Mac OS X don't
come with readline. To get readline support, you can try a stand-alone
readline library[1], or you can use a different Python distribution
(like the one from MacPorts).

[1]: http://pypi.python.org/pypi/readline
"""

# TODO: Disable this stuff for Python 3.4 where readline and rlcompleter are
#       imported by site.py by default.
# TODO: Make NOHIST or something like it available for 3.4+.
# TODO: Make filename the same as 3.4's (~/.python_history). Use same code
#       from stdlib site.py to generate the path.
# TODO: Maybe get rid of source()? Or simplify it by better using inspect()?
# TODO: Implement library to make inspect.getsource() and similar things work
#       on non-pure Python code.
# TODO: Maybe implement that in Python itself and submit a patch?
# TODO: Make use of __text_signature__ when available? Or use what uses it?
#       Then test that os.access() has a signature when doing
#       os.access<ENTER>.
# TODO: Do something about Pygments not being available always. Bundle it?
#       Or just install it on this machine?
# TODO: Better or alternate mechanism for NOHIST? Maybe something that works
#       if you forgot to set it? Like a special exit() function that
#       de-registers the atexit function for writing history maybe? (Which
#       would need a special implementation for 3.4+ vs. older).
# TODO: Make sure everything is correct re: bytes vs. unicode on Python 3?
#       Should we use bytes only? Not worry about locale? Use unicode only?
#       Let Python deal with it somehow? Unicode with errors encoded? ???
def _pythonrc_enable_readline():
    """Enable readline, tab completion, and history"""
    import sys

    try:
        import readline
        import rlcompleter
    except ImportError:
        sys.stderr.write('readline unavailable - tab completion disabled.\n')
        return

    old_complete = readline.get_completer()
    def complete(text, state):
        if not text:
            # Insert four spaces for indentation
            return ('    ', None)[state]
        else:
            return old_complete(text, state)
    readline.parse_and_bind('tab: complete')
    readline.set_completer(complete)

    import atexit
    import os

    # "NOHIST= python" will disable history
    if 'NOHIST' not in os.environ:
        history_path = os.path.expanduser('~/.pyhistory')

        has_written = [False]
        def write_history():
            if not has_written[0]:
                readline.write_history_file(history_path)
                has_written[0] = True
        atexit.register(write_history)

        if os.path.isfile(history_path):
            readline.read_history_file(history_path)
        readline.set_history_length(1000)

def _pythonrc_enable_pprint():
    """Enable pretty printing of evaluated expressions"""
    import pprint
    import sys

    try:
        if sys.platform == 'win32':
            raise ImportError
        from cStringIO import StringIO
        from pygments import highlight
        from pygments.lexers import PythonLexer, PythonTracebackLexer
        from pygments.formatters import Terminal256Formatter

        lexer = PythonLexer()
        tblexer = PythonTracebackLexer()
        formatter = Terminal256Formatter(style='paraiso-light')

        def pphighlight(o, *a, **kw):
            s = pprint.pformat(o, *a, **kw)
            try:
                sys.stdout.write(highlight(s, lexer, formatter))
            except UnicodeError:
                sys.stdout.write(s)
                sys.stdout.write('\n')

        _old_excepthook = sys.excepthook
        def excepthook(exctype, value, traceback):
            """Prints exceptions to sys.stderr and colorizes them"""

            # traceback.format_exception() isn't used because it's
            # inconsistent with the built-in formatter
            old_stderr = sys.stderr
            sys.stderr = StringIO()
            try:
                _old_excepthook(exctype, value, traceback)
                s = sys.stderr.getvalue()
                try:
                    s = highlight(s, tblexer, formatter)
                except UnicodeError:
                    pass
                old_stderr.write(s)
            finally:
                sys.stderr = old_stderr

        sys.excepthook = excepthook
    except ImportError:
        pphighlight = pprint.pprint

    try:
        import __builtin__
    except ImportError:
        import builtins as __builtin__
    import inspect
    import pydoc
    import sys
    import types

    help_types = [types.BuiltinFunctionType, types.BuiltinMethodType,
                  types.FunctionType, types.MethodType, types.ModuleType,
                  type,
                  # method_descriptor
                  type(list.remove)]
    if hasattr(types, 'UnboundMethodType'):
        help_types.append(types.UnboundMethodType)
    help_types = tuple(help_types)

    def _ioctl_width(fd):

        from fcntl import ioctl
        from struct import pack, unpack
        from termios import TIOCGWINSZ
        return unpack('HHHH',
                      ioctl(fd, TIOCGWINSZ, pack('HHHH', 0, 0, 0, 0)))[1]

    def get_width():
        """Returns terminal width"""

        width = 0
        try:
            width = _ioctl_width(0) or _ioctl_width(1) or _ioctl_width(2)
        except ImportError:
            pass
        if not width:
            import os
            width = os.environ.get('COLUMNS', 0)
        return width

    if hasattr(inspect, 'getfullargspec'):
        getargspec = inspect.getfullargspec
    else:
        getargspec = inspect.getargspec

    def pprinthook(value):
        """Pretty print an object to sys.stdout and also save it in
        __builtin__.
        """

        if value is None:
            return
        __builtin__._ = value

        if isinstance(value, help_types):
            reprstr = repr(value)
            try:
                if inspect.isfunction(value):
                    parts = reprstr.split(' ')
                    parts[1] += inspect.formatargspec(*getargspec(value))
                    reprstr = ' '.join(parts)
                elif inspect.ismethod(value):
                    parts = reprstr[:-1].split(' ')
                    parts[2] += inspect.formatargspec(*getargspec(value))
                    reprstr = ' '.join(parts) + '>'
            except TypeError:
                pass
            sys.stdout.write(reprstr)
            sys.stdout.write('\n')
            if getattr(value, '__doc__', None):
                sys.stdout.write('\n')
                sys.stdout.write(pydoc.getdoc(value))
                sys.stdout.write('\n')
        else:
            pphighlight(value, width=get_width() or 80)

    sys.displayhook = pprinthook

def _pythonrc_fix_linecache():
    """Add source(obj) that shows the source code for a given object"""
    import os
    import sys
    from linecache import cache

    # linecache.updatecache() replacement that actually works with zips.
    # See http://bugs.python.org/issue4223 for more information.
    def updatecache(filename, module_globals=None):
        """Update a cache entry and return its list of lines.
        If something's wrong, print a message, discard the cache entry,
        and return an empty list."""

        if filename in cache:
            del cache[filename]
        if not filename or filename[0] + filename[-1] == '<>':
            return []

        fullname = filename
        try:
            stat = os.stat(fullname)
        except os.error:
            basename = os.path.split(filename)[1]

            if module_globals and '__loader__' in module_globals:
                name = module_globals.get('__name__')
                loader = module_globals['__loader__']
                get_source = getattr(loader, 'get_source', None)

                if name and get_source:
                    try:
                        data = get_source(name)
                    except (ImportError, IOError):
                        pass
                    else:
                        if data is None:
                            return []
                        cache[filename] = (
                            len(data), None,
                            [line+'\n' for line in data.splitlines()], fullname
                        )
                        return cache[filename][2]

            for dirname in sys.path:
                try:
                    fullname = os.path.join(dirname, basename)
                except (TypeError, AttributeError):
                    pass
                else:
                    try:
                        stat = os.stat(fullname)
                        break
                    except os.error:
                        pass
            else:
                return []
        try:
            fp = open(fullname, 'rU')
            lines = fp.readlines()
            fp.close()
        except IOError:
            return []
        size, mtime = stat.st_size, stat.st_mtime
        cache[filename] = size, mtime, lines, fullname
        return lines

    import linecache
    linecache.updatecache = updatecache

def _pythonrc_editable_console(*args, **kwargs):
    import os
    import tempfile
    from code import InteractiveConsole

    class EditableConsole(InteractiveConsole):
        def __init__(self, *args, **kwargs):
            self._lastcmd = ''
            InteractiveConsole.__init__(self, *args, **kwargs)

        def runsource(self, source, *args, **kwargs):
            self._lastcmd = source
            return InteractiveConsole.runsource(self, source, *args, **kwargs)

        def raw_input(self, *args, **kwargs):
            line = InteractiveConsole.raw_input(self, *args, **kwargs)
            if line == '\e':
                tmp = tempfile.NamedTemporaryFile(mode='w+', suffix='.py')
                tmp.write(self._lastcmd)
                tmp.flush()
                os.system('%s %s' % (os.environ.get('EDITOR', 'vi'), tmp.name))
                tmp.seek(0)
                for line in tmp:
                    line = line[:-1]
                    self.push(line)
                else:
                    line = ''
            return line

        def write(self, line, *args, **kwargs):
            # Suppress that annoying blank line that
            # interact(banner='') creates. Yes, this is overkill.
            if line != 'I hate banners!\n':
                self.write(line, *args, **kwargs)
            EditableConsole.write = InteractiveConsole.write

    return EditableConsole(*args, **kwargs)

def source(obj):
    """Display the source code of an object.

    Applies syntax highlighting if Pygments is available.
    """
    import os
    import sys
    from inspect import findsource, getmodule, getsource, getsourcefile
    from pydoc import pager

    try:
        # Check to see if the object is defined in a shared library, which
        # findsource() doesn't do properly (see issue4050)
        if not getsourcefile(obj):
            raise TypeError
        s = getsource(obj)
    except TypeError:
        sys.stderr.write("Source code unavailable (maybe it's part of "
                         "a C extension?)\n")
        return

    # TODO: Maybe make this print line by line when not using Pygments? Or
    #       maybe that's dumb? Maybe at least one iterate over the lines once?
    # Detect the module's file encoding. We could use
    # tokenize.detect_encoding(), but it's only available in Python 3.
    import re
    enc = 'ascii'
    for line in findsource(getmodule(obj))[0][:2]:
        m = re.search(r'coding[:=]\s*([-\w.]+)', line)
        if m:
            enc = m.group(1)
    if hasattr(s, 'decode'):
        try:
            s = s.decode(enc, 'replace')
        except LookupError:
            s = s.decode('ascii', 'replace')

    try:
        # For now, let's assume we'll never have a proper terminal on win32
        if sys.platform == 'win32':
            raise ImportError
        from pygments import highlight
        from pygments.lexers import PythonLexer
        from pygments.formatters import Terminal256Formatter
        lexer = PythonLexer()
        formatter = Terminal256Formatter(style='paraiso-light')
        s = highlight(s, lexer, formatter)
    except (ImportError, UnicodeError):
        pass

    # Display the source code in the pager, and try to convince less not to
    # escape color control codes.
    has_lessopts = 'LESS' in os.environ
    lessopts = os.environ.get('LESS', '')
    try:
        os.environ['LESS'] = lessopts + ' -R'
        if hasattr(s, 'decode'):
            pager(s.encode(sys.stdout.encoding, 'replace'))
        else:
            pager(s)
    finally:
        if has_lessopts:
            os.environ['LESS'] = lessopts
        else:
            os.environ.pop('LESS', None)

if __name__ == '__main__':
    __doc__ = None
    if '__file__' in globals():
        del __file__

    # Make sure modules in the current directory can't interfere
    import sys
    try:
        try:
            cwd = sys.path.index('')
            sys.path.pop(cwd)
        except ValueError:
            cwd = None

        # Run installation functions and don't taint the global namespace
        try:
            _pythonrc_enable_readline()
            _pythonrc_enable_pprint()
            _pythonrc_fix_linecache()
            del _pythonrc_enable_readline
            del _pythonrc_enable_pprint
            del _pythonrc_fix_linecache
        finally:
            if cwd is not None:
                sys.path.insert(cwd, '')
            del cwd
    finally:
        del sys

    # Install source()
    try:
        import __builtin__
    except ImportError:
        import builtins as __builtin__
    try:
        __builtin__.source = source
    finally:
        del __builtin__

    # Enable the editable console (trying very hard not to taint the namespace)
    locals().pop('_pythonrc_editable_console')(
        locals=locals()).interact(banner='I hate banners!')

    # Ensure exiting the editable console exits the real console as well
    import sys
    sys.exit()
