// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %platform-sdk-overlay-dir/simd.swiftinterface -emit-module -o %t/simd.swiftmodule -enable-resilience -parse-stdlib -import-module Swift -swift-version 4
// RUN: %target-swift-frontend %platform-sdk-overlay-dir/simd.swiftinterface -emit-module -o %t/simd.swiftmodule -enable-resilience -parse-stdlib -import-module Swift -swift-version 4 -O

// REQUIRES: nonexecutable_test
// REQUIRES: objc_interop
