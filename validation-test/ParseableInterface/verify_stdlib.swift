// Note that this test should still "pass" when Swift.swiftinterface has not
// been generated.

// SWIFT_ENABLE_TENSORFLOW
// FIXME(TF-489): Re-enable this test after fixing `.swiftinterface` errors.
// UNSUPPORTED: nonexecutable_test

// RUN: %empty-directory(%t)
// RUN: not ls %platform-module-dir/Swift.swiftmodule/%target-cpu.swiftinterface || %target-swift-frontend -build-module-from-parseable-interface %platform-module-dir/Swift.swiftmodule/%target-cpu.swiftinterface -parse-stdlib -o %t/Swift.swiftmodule

// REQUIRES: nonexecutable_test
