#!/usr/bin/env python
# utils/update_confusables.py - Utility to update definitions of unicode 
# confusables
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os.path
import re
import sys


def _usage(program_name):
    return 'usage: {}'.format(
        program_name)


def _help(program_name):
    return '{}\n\n'.format(_usage(program_name)) + \
        'This script generates include/swift/Parse/Confusables.def from ' \
        'utils/UnicodeData/confusables.txt.\n' \
        'The latest version of the data file can be found at ' \
        'ftp://ftp.unicode.org/Public/security/latest/confusables.txt.'


def main(args=sys.argv):
    program_name = os.path.basename(args.pop(0))

    if len(args) == 1 and args[0] in ['-h', '--help']:
        print(_help(program_name))
        return 0

    charactersToCheck = [
        u"(", u")", u"{",
        u"}", u"[", u"]",
        u".", u",", u":",
        u";", u"=", u"@",
        u"#", u"&", u"/",
        u"|", u"\\", u"-",
        u"*", u"+", u">",
        u"<", u"!", u"?"
    ]

    modifiedHex = [
        hex(ord(char))[2:].zfill(4).upper() for char in charactersToCheck
    ]

    basepath = os.path.dirname(__file__)
    confusablesFilePath = os.path.abspath(
        os.path.join(basepath, "UnicodeData/confusables.txt")
    )

    pairs = []
    with open(confusablesFilePath, 'r') as f:
        pattern = re.compile(r"(.+)\W+;\W+(.+)\W+;")
        for line in f:
            match = pattern.match(line)
            if match is not None:
                confusedString = match.group(1).replace(" ", "")
                normalString = match.group(2).replace(" ", "")
                for hexValue in modifiedHex:
                    if hexValue == normalString:
                        confused = hex(int(confusedString, 16))
                        normal = hex(int(normalString, 16))
                        pairs.append((confused, normal))

    defFilePath = os.path.abspath(
        os.path.join(basepath, "..", "include/swift/Parse/Confusables.def")
    )
    with open(defFilePath, 'w') as f:
        f.write("//===--- Confusables.def - Confusable unicode characters")
        f.write(" ------------------===//")
        header = '''
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
'''
        f.write(header)
        f.write("//===----------------------------------------------------")
        f.write("------------------===//\n\n")
        f.write("// CONFUSABLE(CONFUSABLE_POINT, BASEPOINT)\n\n")
        for (confused, expected) in pairs:
            f.write("CONFUSABLE(" + confused + ", " + expected + ")\n")
        f.write("\n#undef CONFUSABLE\n")


if __name__ == '__main__':
    main()
