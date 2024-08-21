#!/uar/bin/env python3

import subprocess
import sys


def watchdog(command, timeout=None):
    process = subprocess.Popen(command)
    try:
        process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()
        sys.exit(
            'error: command timed out after {} seconds: {}'
            .format(timeout, ' '.join(sys.argv[2:])))


if __name__ == '__main__':
    watchdog(sys.argv[2:], timeout=float(sys.argv[1]))
