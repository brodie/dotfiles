"""Colorizes diff output for systems with terminfo.

This extension uses the curses library to get ANSI color control codes from
terminfo. It specifically looks for codes for sgr0 (reset), bold, setaf
(foreground color), and setab (background color). See terminfo(5) for more
information.

Default settings:

    [cdiff]
    ; "diff -r rev file"
    head = bold
    ; "@@ -a,b +x,y @@"
    group = bold magenta
    ; -removed line
    del = bold red
    ; +inserted line
    ins = green
    ; -removed line with trailing whitespace
    ; +inserted line with trailing whitespace
    whitespace = bold red

Possible colors: black, red, green, yellow, blue, magenta, cyan, white (and
optionally "bold").

Note: Any trailing whitespace in changed lines is highlighted, even if hasn't
changed between lines.
"""

import os
import sys

from mercurial import hg
from mercurial.commands import diff, table

COLORS = dict(zip(['bold', 'black', 'red', 'green', 'yellow', 'blue',
                   'magenta', 'cyan', 'white'], xrange(-1, 8)))
DEFAULTS = {
    'head': [COLORS['bold']],
    'group': [COLORS['bold'], COLORS['magenta']],
    'del': [COLORS['bold'], COLORS['red']],
    'ins': [COLORS['green']],
    'whitespace': [COLORS['bold'], COLORS['red']],
}

def color_codes(colors):
    """Looks up color control codes using terminfo"""

    from curses import setupterm, tigetstr, tparm
    setupterm()
    reset = tigetstr('sgr0')
    bold = tigetstr('bold')
    fg = tigetstr('setaf')
    bg = tigetstr('setab')
    if None in (reset, bold, fg, bg):
        raise ImportError('Failed to retrieve control codes from terminfo')

    def convert(color, context=fg):
        code = ''
        if COLORS['bold'] in color:
            color.remove(COLORS['bold'])
            code += bold
        if color:
            code += tparm(context, color[0])
        return code

    codes = [convert(colors[c]) for c in ('head', 'group', 'del', 'ins')]
    codes += [convert(colors['whitespace'], bg), reset]
    return codes


def wrap_write(write, head_color, group_color, del_color, ins_color,
               whitespace_color, reset):
    """Wraps ui.write and colorizes diff lines written to it"""

    # This assumes that ui.write is called only with full lines (which is
    # currently the case).
    def wrapper(s):
        lines = s.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('diff'):
                lines[i] = ''.join([head_color, line, reset])
            elif line.startswith('@@'):
                lines[i] = ''.join([group_color, line, reset])
            elif line and line[0] in ('-', '+'):
                # Highlight trailing whitespace (unconditionally)
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
    for key in DEFAULTS:
        colors[key] = ui.config('cdiff', key)
    for key, val in colors.items():
        if val is None:
            colors[key] = DEFAULTS[key]
        else:
            vals = val.split()
            valid = True
            for c in vals:
                if c not in COLORS:
                    ui.warn("cdiff: Unknown color '%s' (in '%s')\n" % (c, key))
                    valid = False
                    break
            if valid:
                colors[key] = [COLORS[c] for c in vals]
            else:
                colors[key] = DEFAULTS[key]

    old_write = ui.write
    ui.write = wrap_write(ui.write, *color_codes(colors))
    try:
        diff(ui, repo, *pats, **opts)
    finally:
        ui.write = old_write

cdiff.__doc__ = diff.__doc__


diffopts = table['^diff']
# This suppresses the useless "extension overrides" warning
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
