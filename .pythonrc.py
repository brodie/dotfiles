def _pythonrc():
    # Enable readline, tab completion, and history
    try:
        import readline
    except ImportError:
        import sys
        sys.stderr.write('readline unavailable - tab completion disabled.\n')
    else:
        import rlcompleter

        old_complete = readline.get_completer()
        def complete(text, state):
            if not text:
                return ('    ', None)[state]
            else:
                return old_complete(text, state)
        readline.parse_and_bind('tab: complete')
        readline.set_completer(complete)

        import atexit
        import os

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

    # Pretty print evaluated expressions

    try:
        import __builtin__
    except ImportError:
        import builtins as __builtin__
    import inspect
    import pprint
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
            pprint.pprint(value, width=get_width() or 80)

    sys.displayhook = pprinthook

    try:
        if sys.platform == 'win32':
            raise ImportError()
        try:
            from cStringIO import StringIO
        except ImportError:
            from StringIO import StringIO
        from pygments import highlight
        from pygments.lexers import PythonTracebackLexer
        from pygments.formatters import TerminalFormatter

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
                s = highlight(s, PythonTracebackLexer(), TerminalFormatter())
                old_stderr.write(s)
            finally:
                sys.stderr = old_stderr

        sys.excepthook = excepthook
    except ImportError:
        pass

    # linecache.updatecache() replacement that actually works with zips
    import os
    import sys
    from linecache import cache
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

# Make sure modules in the current directory can't interfere
import sys
try:
    try:
        cwd = sys.path.index('')
        sys.path.pop(cwd)
    except ValueError:
        cwd = None

    # Run the main function and don't let it taint the global namespace
    try:
        _pythonrc()
        del _pythonrc
    finally:
        if cwd is not None:
            sys.path.insert(cwd, '')
finally:
    del sys

def source(obj):
    """Displays the source code of an object.

    Applies syntax highlighting if Pygments is available.
    """

    import sys

    from inspect import findsource, getmodule, getsource, getsourcefile
    try:
        # Check to see if the object is defined in a shared library, which
        # findsource() doesn't do properly (see issue4050)
        if not getsourcefile(obj):
            raise TypeError()
        s = getsource(obj)
    except TypeError:
        sys.stderr.write("Source code unavailable (maybe it's part of "
                         "a C extension?)\n")
        return

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
        if sys.platform == 'win32':
            raise ImportError()
        from pygments import highlight
        from pygments.lexers import PythonLexer
        from pygments.formatters import TerminalFormatter
        s = highlight(s, PythonLexer(), TerminalFormatter())
    except (ImportError, UnicodeError):
        pass

    import os
    from pydoc import pager
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
