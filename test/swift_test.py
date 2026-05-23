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
import lit.Test

# Like FLAKYPASS, but this one is a failure.
FLAKYFAIL = lit.Test.ResultCode("FLAKYFAIL", "Flaky", True)

class SwiftTest(lit.formats.ShTest, object):
    def __init__(self, coverage_mode=None, execute_external=True):
        super(SwiftTest, self).__init__(execute_external=execute_external)
        if coverage_mode == "FALSE":
            self.coverage_mode = None
        else:
            self.coverage_mode = coverage_mode
        self.skipped_tests = set()

    def before_test(self, test, litConfig):
        if self.coverage_mode:
            # FIXME: The compiler crashers run so fast they fill up the
            # merger's queue (and therefore the build bot's disk)
            if 'crasher' in test.getSourcePath():
                test.config.environment["LLVM_PROFILE_FILE"] = os.devnull
                self.skipped_tests.add(test.getSourcePath())
                return

            if self.coverage_mode == "NOT_MERGED":
                execpath = test.getExecPath()
                profdir = os.path.join(os.path.dirname(execpath), "Output",
                                       os.path.basename(execpath) + '.profdir')
                if not os.path.exists(profdir):
                    os.makedirs(profdir)

                test.config.environment["LLVM_PROFILE_FILE"] = \
                    os.path.join(profdir, "swift-%p.profraw")
            else:
                test.config.environment["LLVM_PROFILE_FILE"] = \
                    os.path.join(test.config.swift_test_results_dir,
                                 "swift-%4m.profraw")

        # If long tests are not run, and this is not a test known to be quite
        # slow, tell Lit to re-run it at most 5 times upon failure to increase
        # our odds detecting non-determinism early. If a test turns out to
        # be flaky, it will fail and be reported as a FLAKYFAIL.
        #
        # NB: Unfortunately, we cannot base this condition on whether a
        # particular test is a long test without hacks because the test is not
        # parsed until it is executed.
        if (
            "long_test" not in test.config.available_features
            # Some of these tests are notoriously slow.
            and not ("Interop" in test.path_in_suite and "Cxx" in test.path_in_suite)
            and not ("swift-dev-utils.test" in test.path_in_suite)
        ):
            test.allowed_retries = 5

    def after_test(self, test, litConfig, result):
        # Intercept FLAKYPASS results and rewrite them into flaky failures. The
        # goal here is to catch non-determinism and complain about it rather
        # than to give a test more chances to succeed.
        if result.code == lit.Test.FLAKYPASS:
            result.code = FLAKYFAIL

        if test.getSourcePath() in self.skipped_tests:
            self.skipped_tests.remove(test.getSourcePath())
        return result

    def execute(self, test, litConfig):
        self.before_test(test, litConfig)
        result = super(SwiftTest, self).execute(test, litConfig)
        return self.after_test(test, litConfig, result)
