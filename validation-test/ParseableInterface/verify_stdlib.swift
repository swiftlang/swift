// Note that this test should still "pass" when Swift.swiftinterface has not
// been generated.

// RUN: %empty-directory(%t)
// RUN: not ls %platform-module-dir/Swift.swiftmodule/%target-cpu.swiftinterface || %target-swift-frontend -build-module-from-parseable-interface %platform-module-dir/Swift.swiftmodule/%target-cpu.swiftinterface -parse-stdlib -o %t/Swift.swiftmodule

// REQUIRES: nonexecutable_test
