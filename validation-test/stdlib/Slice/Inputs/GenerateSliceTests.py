#!/usr/bin/env python

from __future__ import print_function

import sys

def main():
    with open(sys.argv[1], 'r') as f:
        content = f.readlines()

    while len(content) != 0:
        startIndex = -1
        endIndex = -1
        testFilename = ''
        for i, line in enumerate(content):
            if line.startswith('// Start of file: '):
                testFilename = line[18:].strip()
                startIndex = i
                break
        if startIndex == -1:
            return 0
        for i, line in enumerate(content):
            if line.startswith('// End of file: '):
                assert line[16:].strip() == testFilename
                endIndex = i
                break
        testContent = content[startIndex+1:endIndex]

        with open(testFilename, 'w') as testFile:
            for line in testContent:
                testFile.write(line)

        content = content[endIndex+1:]

if __name__ == "__main__":
    sys.exit(main())

