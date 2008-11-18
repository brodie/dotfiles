# color.py color output for the status and qseries commands
#
# Copyright (C) 2007 Kevin Christen <kevin.christen@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

'''add color output to status, qseries, and diff-related commands

This extension modifies the status command to add color to its output to
reflect file status, the qseries command to add color to reflect patch status
(applied, unapplied, missing), and to diff-related commands to highlight
additions, removals, diff headers, and trailing whitespace.  Other effects in
addition to color, like bold and underlined text, are also available.
Effects are rendered with the ECMA-48 SGR control function (aka ANSI escape
codes).  This module also provides the render_text function, which can be
used to add effects to any text.

To enable this extension, add this to your .hgrc file:
[extensions]
color =

Default effects my be overriden from the .hgrc file:

[color]
status.modified = blue bold underline red_background
status.added = green bold
status.removed = red bold blue_background
status.deleted = cyan bold underline
status.unknown = magenta bold underline
status.ignored = black bold

# 'none' turns off all effects
status.clean = none
status.copied = none

qseries.applied = blue bold underline
qseries.unapplied = black bold
qseries.missing = red bold

diff.head = bold
diff.group = magenta bold
diff.del = red bold
diff.ins = green
diff.whitespace = bold red_background
'''

import os, re, sys

from mercurial import commands, extensions
from mercurial.i18n import _

# start and stop parameters for effects
_effect_params = { 'none': (0, 0),
                   'black': (30, 39),
                   'red': (31, 39),
                   'green': (32, 39),
                   'yellow': (33, 39),
                   'blue': (34, 39),
                   'magenta': (35, 39),
                   'cyan': (36, 39),
                   'white': (37, 39),
                   'bold': (1, 22),
                   'italic': (3, 23),
                   'underline': (4, 24),
                   'inverse': (7, 27),
                   'black_background': (40, 49),
                   'red_background': (41, 49),
                   'green_background': (42, 49),
                   'yellow_background': (43, 49),
                   'blue_background': (44, 49),
                   'purple_background': (45, 49),
                   'cyan_background': (46, 49),
                   'white_background': (47, 49), }

def render_effects(text, *effects):
    'Wrap text in commands to turn on each effect.'
    start = [ str(_effect_params['none'][0]) ]
    stop = []
    for effect in effects:
        start.append(str(_effect_params[effect][0]))
        stop.append(str(_effect_params[effect][1]))
    stop.append(str(_effect_params['none'][1]))
    start = '\033[' + ';'.join(start) + 'm'
    stop = '\033[' + ';'.join(stop) + 'm'
    return start + text + stop

def colorstatus(orig, ui, repo, *pats, **opts):
    '''run the status command with colored output'''

    delimiter = opts['print0'] and '\0' or '\n'

    # run status and capture it's output
    ui.pushbuffer()
    retval = orig(ui, repo, *pats, **opts)
    # filter out empty strings
    lines = [ line for line in ui.popbuffer().split(delimiter) if line ]

    if opts['no_status']:
        # if --no-status, run the command again without that option to get
        # output with status abbreviations
        opts['no_status'] = False
        ui.pushbuffer()
        orig(ui, repo, *pats, **opts)
        # filter out empty strings
        lines_with_status = [ line for
                              line in ui.popbuffer().split(delimiter) if line ]
    else:
        lines_with_status = lines

    # apply color to output and display it
    for i in xrange(0, len(lines)):
        status = _status_abbreviations[lines_with_status[i][0]]
        effects = _status_effects[status]
        if effects:
            lines[i] = render_effects(lines[i], *effects)
        sys.stdout.write(lines[i] + delimiter)
    return retval

_status_abbreviations = { 'M': 'modified',
                          'A': 'added',
                          'R': 'removed',
                          '!': 'deleted',
                          '?': 'unknown',
                          'I': 'ignored',
                          'C': 'clean',
                          ' ': 'copied', }

_status_effects = { 'modified': ('blue', 'bold'),
                    'added': ('green', 'bold'),
                    'removed': ('red', 'bold'),
                    'deleted': ('cyan', 'bold', 'underline'),
                    'unknown': ('magenta', 'bold', 'underline'),
                    'ignored': ('black', 'bold'),
                    'clean': ('none', ),
                    'copied': ('none', ), }

def colorqseries(orig, ui, repo, *dummy, **opts):
    '''run the qseries command with colored output'''
    ui.pushbuffer()
    retval = orig(ui, repo, **opts)
    patches = ui.popbuffer().splitlines()
    for patch in patches:
        patchname = patch
        if opts['summary']:
            patchname = patchname.split(': ')[0]
        if ui.verbose:
            patchname = patchname.split(' ', 2)[-1]

        if opts['missing']:
            effects = _patch_effects['missing']
        # Determine if patch is applied.
        elif [ applied for applied in repo.mq.applied
               if patchname == applied.name ]:
            effects = _patch_effects['applied']
        else:
            effects = _patch_effects['unapplied']
        sys.stdout.write(render_effects(patch, *effects) + '\n')
    return retval

_patch_effects = { 'applied': ('blue', 'bold', 'underline'),
                   'missing': ('red', 'bold'),
                   'unapplied': ('black', 'bold'), }

def colordiff(orig, ui, repo, *dummy, **opts):
    '''run diff-related commands with colored output'''

    from mercurial.commands import incoming, log, outgoing, tip
    if orig in (incoming, log, outgoing, tip) and not opts['patch']:
        return orig(ui, repo, *dummy, **opts)

    # This assumes that ui.write is called only with full lines (which is
    # currently the case).
    def wrapper(orig, s):
        lines = s.split('\n')
        for i, line in enumerate(lines):
            if line.startswith('diff'):
                lines[i] = render_effects(line, *_diff_effects['head'])
            elif line.startswith('@@'):
                lines[i] = render_effects(line, *_diff_effects['group'])
            elif line and line[0] in ('-', '+'):
                # Highlight trailing whitespace (unconditionally)
                rline = line.rstrip()
                if line != rline:
                    pos = len(rline)
                    line = ''.join([line[:pos],
                        render_effects(line[pos:],
                                       *_diff_effects['whitespace'])])
                if line[0] == '-':
                    lines[i] = render_effects(line, *_diff_effects['del'])
                else:
                    lines[i] = render_effects(line, *_diff_effects['ins'])
        orig('\n'.join(lines))

    old_write = extensions.wrapfunction(ui, 'write', wrapper)
    try:
        orig(ui, repo, *dummy, **opts)
    finally:
        ui.write = old_write

_diff_effects = { 'head': ('bold', ),
                  'group': ('magenta', 'bold'),
                  'del': ('red', 'bold'),
                  'ins': ('green', ),
                  'whitespace': ('bold', 'red_background'), }

def uisetup(ui):
    '''Initialize the extension.'''
    _setupcmd(ui, 'diff', commands.table, colordiff, _diff_effects)
    _setupcmd(ui, 'incoming', commands.table, colordiff, _diff_effects)
    _setupcmd(ui, 'log', commands.table, colordiff, _diff_effects)
    _setupcmd(ui, 'outgoing', commands.table, colordiff, _diff_effects)
    _setupcmd(ui, 'tip', commands.table, colordiff, _diff_effects)
    _setupcmd(ui, 'status', commands.table, colorstatus, _status_effects)
    if ui.config('extensions', 'hgext.mq') is not None or \
            ui.config('extensions', 'mq') is not None:
        from hgext import mq
        _setupcmd(ui, 'qdiff', mq.cmdtable, colordiff, _diff_effects)
        _setupcmd(ui, 'qseries', mq.cmdtable, colorqseries, _patch_effects)

def _setupcmd(ui, cmd, table, func, effectsmap):
    '''patch in command to command table and load effect map'''
    def nocolor(orig, *args, **opts):
        # Duplicate stdout in case sys.stdout has been reassigned
        try:
            stdout = os.fdopen(os.dup(1), 'w')
            try:
                isatty = stdout.isatty()
            finally:
                stdout.close()
        except Exception:
            isatty = sys.stdout.isatty()

        if (opts['no_color'] or opts['color'] == 'never' or
            (opts['color'] == 'auto' and
             (os.environ.get('TERM') == 'dumb' or not isatty))):
            return orig(*args, **opts)
        else:
            return func(orig, *args, **opts)

    entry = extensions.wrapcommand(table, cmd, nocolor)
    entry[1].append(('', 'color', 'auto',
                     _("when to colorize (always, auto, or never)")))
    entry[1].append(('', 'no-color', None, _("don't colorize output")))

    for status in effectsmap:
        effects = ui.config('color', cmd + '.' + status)
        if effects:
            effectsmap[status] = re.split('\W+', effects)