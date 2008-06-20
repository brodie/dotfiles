def _pythonrc():
    # Enable readline, tab completion, and history

    import rlcompleter
    import readline

    class TabCompleter(rlcompleter.Completer):
        """Completer that supports indenting"""

        def complete(self, text, state):
            if not text:
                return ('    ', None)[state]
            else:
                return rlcompleter.Completer.complete(self, text, state)

    readline.parse_and_bind('tab: complete')
    readline.set_completer(TabCompleter().complete)

    import atexit
    import os

    history_path = os.path.expanduser('~/.pyhistory')
    atexit.register(lambda: readline.write_history_file(history_path))
    if os.path.isfile(history_path):
        readline.read_history_file(history_path)

    # Pretty print evaluated expressions

    import __builtin__
    import pprint
    import pydoc
    import sys
    import types

    help_types = (types.BuiltinFunctionType, types.BuiltinMethodType,
                  types.FunctionType, types.MethodType, types.ModuleType,
                  types.TypeType, types.UnboundMethodType,
                  # method_descriptor
                  type(list.remove))

    def pprinthook(value):
        """Pretty print an object to sys.stdout and also save it in
        __builtin__.
        """

        if value is not None:
            if (isinstance(value, help_types) and
                getattr(value, '__doc__', None)):
                print repr(value)
                print
                print pydoc.getdoc(value)
            else:
                pprint.pprint(value)
        __builtin__._ = value

    sys.displayhook = pprinthook


# Run the main function and don't let it taint the global namespace
_pythonrc()
del _pythonrc
