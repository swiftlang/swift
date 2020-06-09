#!/uar/bin/env python

import subprocess
import sys
import threading


def watchdog(command, timeout=None):
    process = subprocess.Popen(command)
    timer = threading.Timer(timeout, process.kill)
    try:
        timer.start()
        process.communicate()
    finally:
        timer.cancel()


if __name__ == '__main__':
    watchdog(sys.argv[2:], timeout=float(sys.argv[1]))
