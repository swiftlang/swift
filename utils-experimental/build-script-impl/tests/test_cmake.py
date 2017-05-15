# tests/test_cmake.py -------------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import unittest

from build_script.cmake import CMakeOptions


class CMakeTestCase(unittest.TestCase):

    def test_cmake(self):
        pass

    def test_cmakeoptions(self):
        options = CMakeOptions()
        options.define('OPT1:STRING', 'foo')
        options.define('OPT2:BOOL', 1)
        options.define('OPT3:BOOL', '1')
        options.define('OPT4:BOOL', 'true')
        options.define('OPT5:BOOL', 'True')
        options.define('OPT6:BOOL', True)
        options.define('OPT7:BOOL', 0)
        options.define('OPT8:BOOL', '0')
        options.define('OPT9:BOOL', 'false')
        options.define('OPT10:BOOL', 'False')
        options.define('OPT11:BOOL', False)
        options.define('OPT12', 12)
        options.define('OPT13', '')
        options.define('OPT14', None)
        options.define('OPT15:PATH', 'foo')

        options.unset('OPT16:BOOL')
        options.unset('OPT17:STRING')
        options.unset('OPT18')

        self.assertRaises(ValueError, options.define, 'ERR', ["FOO"])
        self.assertRaises(ValueError, options.define, 'ERR', {"FOO": 1})

        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 3)
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 'foo')
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', [1])
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 'YES')
        self.assertRaises(ValueError, options.define, 'ERR:BOOL', 'NO')

        self.assertEqual(list(options), [
            '-DOPT1:STRING=foo',
            '-DOPT2:BOOL=TRUE',
            '-DOPT3:BOOL=TRUE',
            '-DOPT4:BOOL=TRUE',
            '-DOPT5:BOOL=TRUE',
            '-DOPT6:BOOL=TRUE',
            '-DOPT7:BOOL=FALSE',
            '-DOPT8:BOOL=FALSE',
            '-DOPT9:BOOL=FALSE',
            '-DOPT10:BOOL=FALSE',
            '-DOPT11:BOOL=FALSE',
            '-DOPT12=12',
            '-DOPT13=',
            '-DOPT14=',
            '-DOPT15:PATH=foo',
            '-UOPT16',
            '-UOPT17',
            '-UOPT18'])

    def test_cmakeoptions_op(self):

        options1 = CMakeOptions()
        options1.define("OPT1_1", 'VAL1')
        options1.define("OPT1_2", 'VAL2')

        options2 = CMakeOptions()
        options2.define("OPT2_1", 'VAL3')

        options = options1 + options2
        self.assertEqual(list(options), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3"])

        options += options2
        self.assertEqual(list(options), [
            "-DOPT1_1=VAL1",
            "-DOPT1_2=VAL2",
            "-DOPT2_1=VAL3",
            "-DOPT2_1=VAL3"])
