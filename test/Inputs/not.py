import shlex
import subprocess
import sys

if len(sys.argv) < 2:
    print("Too few args to " + sys.argv[0])
    sys.exit(0)

try:
    isPosix = (sys.platform != "win32")
    subprocess.check_call(shlex.split(sys.argv[1], posix=isPosix))
    sys.exit(1)
except subprocess.CalledProcessError:
    sys.exit(0)
