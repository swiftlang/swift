#!/usr/bin/env python

import subprocess
import sys

for line in sys.stdin:
    subprocess.check_call(line.split())
