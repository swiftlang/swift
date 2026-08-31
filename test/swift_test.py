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

# How many times to run a retry-eligible test before giving up on it.
MAX_ATTEMPTS = 6

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
        # slow, re-run it up to MAX_ATTEMPTS times upon failure to increase our
        # odds of detecting non-determinism early. If a test turns out to be
        # flaky, it will fail and be reported as a FLAKYFAIL.
        #
        # `execute` drives the retries rather than Lit's own retry loop, which
        # reports the output of the last attempt. For a flake that is the
        # attempt that passed, leaving "Exit Code: 0" and clean output as the
        # only evidence of the non-determinism this mechanism exists to catch.
        #
        # NB: Unfortunately, we cannot base this condition on whether a
        # particular test is a long test without hacks because the test is not
        # parsed until it is executed.
        if (
            "long_test" not in test.config.available_features
            # Some of these tests are notoriously slow.
            and not ("Interop" in test.path_in_suite and "Cxx" in test.path_in_suite)
            and not ("swift-dev-utils.test" in test.path_in_suite)
            # TODO: remove once the number of XFAILs under opaque values is close to zero.
            and not ("swift_test_mode_optimize_none_with_opaque_values" in test.config.available_features)
        ):
            test.swift_max_attempts = MAX_ATTEMPTS

    def after_test(self, test, litConfig, result):
        # Intercept FLAKYPASS results and rewrite them into flaky failures. The
        # goal here is to catch non-determinism and complain about it rather
        # than to give a test more chances to succeed.
        #
        # execute_with_retries never returns FLAKYPASS, so this only fires for a
        # test that reached Lit's own retry loop via ALLOW_RETRIES. FLAKYPASS
        # counts as a pass even though the test failed, and we'd prefer to
        # expose those as test failures. Such a test keeps the output of the
        # attempt that passed, so it does not get the reporting improvement
        # above, so Swift tests should avoid ALLOW_RETRIES.
        if result.code == lit.Test.FLAKYPASS:
            result.code = FLAKYFAIL

        if test.getSourcePath() in self.skipped_tests:
            self.skipped_tests.remove(test.getSourcePath())
        return result

    def execute(self, test, litConfig):
        self.before_test(test, litConfig)
        result = self.execute_with_retries(test, litConfig)
        return self.after_test(test, litConfig, result)

    def execute_with_retries(self, test, litConfig):
        max_attempts = getattr(test, "swift_max_attempts", 1)
        first_failure = None
        elapsed = 0.0

        for attempt in range(1, max_attempts + 1):
            result = super(SwiftTest, self).execute(test, litConfig)
            elapsed += result.elapsed or 0.0
            if result.code != lit.Test.FAIL:
                break
            if first_failure is None:
                first_failure = result

        if first_failure is None:
            return result

        # The test failed and then passed, so what went wrong is in the output
        # of the attempt that failed. Report that, not the passing run.
        if result.code != lit.Test.FAIL:
            output = (
                "Test failed on attempt 1 of %d and passed on attempt %d. "
                "Output of the failing attempt follows.\n\n%s"
                % (max_attempts, attempt, first_failure.output)
            )
            result = lit.Test.Result(FLAKYFAIL, output, elapsed)

        result.attempts = attempt
        result.max_allowed_attempts = max_attempts
        return result
