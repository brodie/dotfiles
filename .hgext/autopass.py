import os
import socket
import sys

from mercurial import httprepo
from mercurial.httprepo import passwordmgr as old_passwordmgr

def _getcreds():

    path = os.path.expanduser('~/.hgpass')
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        s.connect(path)
        s.send('?')
        return s.recv(1024).split('\x00')
    finally:
        s.close()


class autopasswordmgr(old_passwordmgr):

    def __init__(self, ui):
        old_passwordmgr.__init__(self, ui)
        try:
            self._user, self._pass = _getcreds()
        except Exception, e:
            self._user, self._pass = None, None, None

    def find_user_password(self, realm, authuri):

        if (authuri.startswith(self.ui.config('autopass', 'uri')) and
                               None not in (self._user, self._pass)):
            return self._user, self._pass
        else:
            return old_passwordmgr.find_user_password(self, realm, authuri)

httprepo.passwordmgr = autopasswordmgr
