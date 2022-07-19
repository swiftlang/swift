import sys


def error(msg):
    print('error: ' + msg, file=sys.stderr)
    sys.exit(-1)
