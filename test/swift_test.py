# swift/test/swift_test.py - SwiftTest format for lit tests -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# -----------------------------------------------------------------------------
#
# This is a test format file for the 'lit' test runner.
#
# -----------------------------------------------------------------------------

import os

import lit
import lit.formats
import lit.util


class SwiftTest(lit.formats.ShTest, object):
    def __init__(self, coverage_mode=None, execute_external=True):
        super(SwiftTest, self).__init__(execute_external=execute_external)
        if coverage_mode == "FALSE":
            self.coverage_mode = None
        else:
            self.coverage_mode = coverage_mode
        self.skipped_tests = set()

    def before_test(self, test, litConfig):
        _, tmp_base = lit.TestRunner.getTempPaths(test)

        # Apparently despite the docs, tmpDir is not actually unique for each
        # test, but tmpBase is. Remove it here and add a tmpBase substitution
        # in before_test. Speculative fix for rdar://32928464.
        try:
            percentT_index = \
                [x[0] for x in test.config.substitutions].index('%T')
        except ValueError:
            pass
        else:
            test.config.substitutions.pop(percentT_index)

            test.config.substitutions.append(
                ('%T', '$(mkdir -p %r && echo %r)' % (tmp_base, tmp_base)))

        if self.coverage_mode:
            # FIXME: The compiler crashers run so fast they fill up the
            # merger's queue (and therefore the build bot's disk)
            if 'crasher' in test.getSourcePath():
                test.config.environment["LLVM_PROFILE_FILE"] = os.devnull
                self.skipped_tests.add(test.getSourcePath())
                return

            if self.coverage_mode == "NOT_MERGED":
                profdir = tmp_base + '.profdir'
                if not os.path.exists(profdir):
                    os.makedirs(profdir)

                test.config.environment["LLVM_PROFILE_FILE"] = \
                    os.path.join(profdir, "swift-%p.profraw")
            else:
                test.config.environment["LLVM_PROFILE_FILE"] = \
                    os.path.join(test.config.swift_test_results_dir,
                                 "swift-%4m.profraw")

    def after_test(self, test, litConfig, result):
        if test.getSourcePath() in self.skipped_tests:
            self.skipped_tests.remove(test.getSourcePath())
        return result

    def execute(self, test, litConfig):
        self.before_test(test, litConfig)
        result = super(SwiftTest, self).execute(test, litConfig)
        return self.after_test(test, litConfig, result)
