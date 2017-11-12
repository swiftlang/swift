#!/usr/bin/env python

from __future__ import print_function

import os
import sys

print(os.path.getmtime(sys.argv[1]))
