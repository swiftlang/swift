#!/usr/bin/env python3

import subprocess
import sys

for line in sys.stdin:
    subprocess.check_call(line.split())
