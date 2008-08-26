import os
import socket
import sys

def _getpass(prompt='Password: ', stream=sys.stdout):

    path = os.path.expanduser('~/.hgpass')
    s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        s.connect(path)
        s.send('?')
        password = s.recv(1024)
    finally:
        s.close()

    stream.write(prompt)
    stream.write('<automatically supplied>\n')
    return password


from getpass import getpass as _old_getpass
def _wrapper(*args, **kwargs):

    try:
        return _getpass(*args, **kwargs)
    except Exception:
        return _old_getpass(*args, **kwargs)


import getpass
getpass.getpass = _wrapper


if __name__ == '__main__':
    print repr(getpass())
