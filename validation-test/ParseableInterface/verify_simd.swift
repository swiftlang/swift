// Note that this test should still "pass" when simd.swiftinterface has not been
// generated.

// RUN: %empty-directory(%t)
// RUN: test ! -e %platform-sdk-overlay-dir/simd.swiftinterface || %target-swift-frontend %platform-sdk-overlay-dir/simd.swiftinterface -emit-module -o %t/simd.swiftmodule -enable-resilience -parse-stdlib -import-module Swift -swift-version 4
// RUN: test ! -e %platform-sdk-overlay-dir/simd.swiftinterface || %target-swift-frontend %platform-sdk-overlay-dir/simd.swiftinterface -emit-module -o %t/simd.swiftmodule -enable-resilience -parse-stdlib -import-module Swift -swift-version 4 -O

// REQUIRES: nonexecutable_test
// REQUIRES: objc_interop
