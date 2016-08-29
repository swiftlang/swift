# RUN: %{python} %s
from __future__ import print_function

import time
import sys

print("Running infinite loop")
sys.stdout.flush() # Make sure the print gets flushed so it appears in lit output.
while True:
    pass
