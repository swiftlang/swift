# RUN: %{python} %s
from __future__ import print_function

import time
import sys

print("Running slow program")
sys.stdout.flush() # Make sure the print gets flushed so it appears in lit output.
time.sleep(6)
