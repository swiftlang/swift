#!/uar/bin/env python3

import platform
import subprocess
import sys

sampleCommand = None
timeoutSampleTime = 0

if platform.system() == 'Darwin':
    sampleCommand = '/usr/bin/sample'
    timeoutSampleTime = 10


def watchdog(command, timeout):
    process = subprocess.Popen(command)
    try:
        process.wait(timeout=timeout)
    except subprocess.TimeoutExpired:
        if sampleCommand:
            pidstr = str(process.pid)
            subprocess.run([sampleCommand, pidstr, str(timeoutSampleTime)])
        process.kill()
        sys.exit(
            'error: command timed out after {} seconds: {}'
            .format(timeout, ' '.join(sys.argv[2:])))


if __name__ == '__main__':
    watchdog(sys.argv[2:], timeout=float(sys.argv[1]))
