"""Colorizes diff/qdiff output for Unix terminals.

This extension uses ANSI color control codes to colorize output from the
built-in diff command and MQ's qdiff command. It highlights diff headings and
changed lines, much like the Python library Pygments and the command line
utility colordiff do.

The --color switch may be used to control when colorizing occurs. Possible
values are "auto", "always", and "never". "auto" enables colorization only
for non-dumb terminals (a la GNU grep).

Note: If auto-colorization is enabled and you're using the pager extension
with less, make sure less is run with the -R switch, so it won't escape color
control codes.

Default settings:

    [cdiff]
    ; --color default
    color = auto
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

Note: Any trailing whitespace in changed lines is highlighted, even if it
hasn't changed between lines.
"""

import os
import sys

from mercurial import commands, extensions
from mercurial.ui import ui as ui_cls

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


def colorize(color, line, reset):
    """Colorizes a line, preserving any trailing carriage return"""

    if line.endswith('\r'):
        return ''.join([color, line[:-1], reset, '\r'])
    else:
        return ''.join([color, line, reset])


def wrap_write(write, head_color, group_color, del_color, ins_color,
               whitespace_color, reset):
    """Wraps ui.write and colorizes diff lines written to it"""

    # This assumes that ui.write is called only with full lines (which is
    # currently the case).
    def wrapper(s):
        lines = s.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('diff'):
                lines[i] = colorize(head_color, line, reset)
            elif line.startswith('@@'):
                lines[i] = colorize(group_color, line, reset)
            elif line and line[0] in ('-', '+'):
                # Highlight trailing whitespace (unconditionally)
                rline = line.rstrip()
                if line != rline:
                    pos = len(rline)
                    line = ''.join([line[:pos], colorize(whitespace_color,
                                                         line[pos:], reset)])
                if line[0] == '-':
                    lines[i] = colorize(del_color, line, reset)
                else:
                    lines[i] = colorize(ins_color, line, reset)
        write('\n'.join(lines))
    return wrapper


def cdiff(orig, ui, repo, *pats, **opts):
    """Colorized diff"""

    # Duplicate stdout in case sys.stdout has been reassigned
    try:
        stdout = os.fdopen(os.dup(1), 'w')
        try:
            isatty = stdout.isatty()
        finally:
            stdout.close()
    except Exception:
        isatty = sys.stdout.isatty()

    if (opts['color'] == 'never' or
        (opts['color'] == 'auto' and
         (os.environ.get('TERM') == 'dumb' or not isatty))):
        orig(ui, repo, *pats, **opts)
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
        orig(ui, repo, *pats, **opts)
    finally:
        ui.write = old_write


def uisetup(ui):
    """Adds -c/--color to the diff and qdiff commands"""

    default = 'auto'
    color = ui.config('cdiff', 'color')
    if color in ('always', 'never'):
        default = color
    cdiffopts = ('c', 'color', default,
                 'when to colorize (always, auto, or never)')

    # Try to find mq, or load it if it's not already loaded
    mq = None
    try:
        mq = extensions.find('mq')
    except KeyError:
        path = ui.config('extensions', 'hgext.mq',
                         ui.config('extensions', 'mq'))
        if path is not None:
            # extensions.loadall() is used instead of load() because it
            # does some extra path parsing that load() doesn't.
            class ui_wrapper(ui_cls):
                def configitems(self, section, untrusted=False):
                    if section == 'extensions':
                        return [('mq', path)]
                    else:
                        return ui_cls.configitems(self, section, untrusted)
            _ui = ui_wrapper(parentui=ui)
            extensions.loadall(_ui)
            mq = extensions.find('mq')

    # hg log -p/--patch can be colorized, but non-diff messages may get
    # colorized inadvertently at the moment.
    cmds = [('diff', commands.table), ('log', commands.table)]
    if mq is not None:
        cmds.append(('qdiff', mq.cmdtable))

    for name, table in cmds:
        entry = extensions.wrapcommand(table, name, cdiff)
        entry[1].append(cdiffopts)
