#!/usr/bin/env python

from __future__ import print_function

import subprocess
import sys
import threading


def watchdog(command, timeout=None):
    process = subprocess.Popen(command)

    def process_kill():
        process.kill()
        print("Timeout!", file=sys.stderr)

    timer = threading.Timer(timeout, process_kill)
    try:
        timer.start()
        process.communicate()
    finally:
        timer.cancel()


if __name__ == '__main__':
    watchdog(sys.argv[2:], timeout=float(sys.argv[1]))
