
// The compiler should finish in about 5 seconds. To give some slack,
// specify a timeout of 60 seconds
// If the compiler needs more than 60 seconds, there is probably a real problem.
// So please don't just increase the timeout in case this test fails.

// RUN: %{python} %S/../../test/Inputs/timeout.py 60 %target-swift-frontend -O -parse-as-library -sil-verify-none -emit-sil %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: CPU=arm64 || CPU=x86_64 || CPU=aarch64
// REQUIRES: OS=macosx

import Darwin

// Check that redundant-load-elimination and dead-store-elimination don't take
// extremely long when optimizing statfs, which contains a 1023-element tuple.

// CHECK-LABEL: test_rle_dse_compile_time
public func test_rle_dse_compile_time(_ s: statfs) {
  withUnsafePointer(to: s.f_mntonname) {
    $0.withMemoryRebound(to: UInt8.self, capacity: Int(MAXPATHLEN)) { (str) in
      print(str)
    }
  }
}

