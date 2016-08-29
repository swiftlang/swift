# RUN: %{python} %s quick
# RUN: %{python} %s slow
from __future__ import print_function

import time
import sys

if len(sys.argv) != 2:
    print("Wrong number of args")
    sys.exit(1)

mode =  sys.argv[1]

if mode == 'slow':
    print("Running in slow mode")
    sys.stdout.flush() # Make sure the print gets flushed so it appears in lit output.
    time.sleep(6)
    sys.exit(0)
elif mode == 'quick':
    print("Running in quick mode")
    sys.exit(0)
else:
    print("Unrecognised mode {}".format(mode))
    sys.exit(1)
