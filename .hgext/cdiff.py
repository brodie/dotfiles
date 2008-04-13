#!/usr/bin/env python
"""Colorizes diff output"""

import os
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
        (opts['color'] == 'auto' and
         (os.environ.get('TERM') == 'dumb' or not sys.stdout.isatty())
        )
    ):
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
    try:
        output = highlight(output.decode('utf-8'), DiffLexer(),
                           TerminalFormatter(encoding='utf-8'))
        ui.write(output)
    except UnicodeError:
        ui.write(output)

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
