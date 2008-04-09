#!/usr/bin/env python
"""Colorizes diff output"""

import sys

try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO

from mercurial import hg
from mercurial.commands import diff, table
from pygments import highlight
from pygments.lexers import DiffLexer
from pygments.formatters import TerminalFormatter

def cdiff(ui, repo, *pats, **opts):
    """Colorized diff"""

    if (opts['color'] == 'never' or
        (opts['color'] == 'auto' and not sys.stdout.isatty())):
        diff(ui, repo, *pats, **opts)
        return

    stdout = sys.stdout
    sys.stdout = StringIO()
    try:
        diff(ui, repo, *pats, **opts)
        output = sys.stdout.getvalue()
    finally:
        sys.stdout.close()
        sys.stdout = stdout
    ui.write(highlight(output, DiffLexer(), TerminalFormatter()))

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
