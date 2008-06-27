#!/usr/bin/env python
"""Colorizes diff output"""

import os
import sys

from mercurial import hg
from mercurial.commands import diff, table

def wrap_write(write):
    """Wraps ui.write and colorizes diff lines written to it"""

    def wrapper(s):
        lines = s.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('diff'):
                lines[i] = ''.join(['\x1b[01m', line, '\x1b[0m'])
            elif line.startswith('---'):
                lines[i] = ''.join(['\x1b[1;31m', line, '\x1b[0m'])
            elif line.startswith('+++'):
                lines[i] = ''.join(['\x1b[32m', line, '\x1b[0m'])
            elif line.startswith('@@'):
                lines[i] = ''.join(['\x1b[01m\x1b[35m', line, '\x1b[0m'])
            elif line.startswith('-'):
                lines[i] = ''.join(['\x1b[31m', line, '\x1b[0m'])
            elif line.startswith('+'):
                lines[i] = ''.join(['\x1b[32m', line, '\x1b[0m'])
        write('\n'.join(lines))
    return wrapper


def cdiff(ui, repo, *pats, **opts):
    """Colorized diff"""

    if (opts['color'] == 'never' or
        (opts['color'] == 'auto' and
         (os.environ.get('TERM') == 'dumb' or not sys.stdout.isatty()))):
        diff(ui, repo, *pats, **opts)
        return

    old_write = ui.write
    ui.write = wrap_write(ui.write)
    try:
        diff(ui, repo, *pats, **opts)
    finally:
        ui.write = old_write

cdiff.__doc__ = diff.__doc__


diffopts = table['^diff']
del table['^diff']
cmdtable = {'^diff': (
    cdiff,
    diffopts[1] + [(
        'c',
        'color',
        'auto',
        'when to colorize (always, auto, or never)',
    )],
    diffopts[2],
)}
