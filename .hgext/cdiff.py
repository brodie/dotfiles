#!/usr/bin/env python
"""Colorizes diff output"""

import os
import sys

from mercurial import hg
from mercurial.commands import diff, table

# FIXME: Look up colors using curses
COLORS = {
    'head_color': '\x1b[01m',
    'group_color': '\x1b[01m\x1b[35m',
    'del_color': '\x1b[1;31m',
    'ins_color': '\x1b[32m',
    'whitespace_color': '\x1b[01m\x1b[41m',
}

def wrap_write(write, head_color, group_color, del_color, ins_color,
               whitespace_color, reset='\x1b[0m'):
    """Wraps ui.write and colorizes diff lines written to it"""

    def wrapper(s):
        lines = s.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('diff'):
                lines[i] = ''.join([head_color, line, reset])
            elif line.startswith('@@'):
                lines[i] = ''.join([group_color, line, reset])
            elif line and line[0] in ('-', '+'):
                # Highlight trailing whitespace
                rline = line.rstrip()
                if line != rline:
                    pos = len(rline)
                    line = ''.join([line[:pos], whitespace_color,
                                    line[pos:], reset])
                if line[0] == '-':
                    lines[i] = ''.join([del_color, line, reset])
                else:
                    lines[i] = ''.join([ins_color, line, reset])
        write('\n'.join(lines))
    return wrapper


def cdiff(ui, repo, *pats, **opts):
    """Colorized diff"""

    if (opts['color'] == 'never' or
        (opts['color'] == 'auto' and
         (os.environ.get('TERM') == 'dumb' or not sys.stdout.isatty()))):
        diff(ui, repo, *pats, **opts)
        return

    colors = {}
    for key in COLORS:
        colors[key] = ui.config('cdiff', key)
    for key, val in colors.items():
        if val is None:
            colors[key] = COLORS[key]
        else:
            colors[key] = val.replace('^[', '\x1b')

    old_write = ui.write
    ui.write = wrap_write(ui.write, **colors)
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
