// Note that this test should still "pass" when simd.swiftinterface has not been
// generated.

// RUN: %empty-directory(%t)
// RUN: test ! -e %platform-sdk-overlay-dir/simd.swiftmodule/%target-cpu.swiftinterface || %target-swift-frontend -build-module-from-parseable-interface %platform-sdk-overlay-dir/simd.swiftmodule/%target-cpu.swiftinterface -o %t/simd.swiftmodule

// REQUIRES: nonexecutable_test
// REQUIRES: objc_interop
