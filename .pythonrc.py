def _pythonrc():
    # Enable readline and tab completion

    import rlcompleter
    import readline

    readline.parse_and_bind('tab: complete')

    # Pretty print evaluated expressions

    import __builtin__
    import pprint
    import sys

    def pprinthook(value):
        """Pretty print an object to sys.stdout and also save it in __builtin__"""

        if value is not None:
            pprint.pprint(value)
        __builtin__._ = value

    sys.displayhook = pprinthook


# Run the main function and don't let it taint the global namespace
_pythonrc()
del _pythonrc
