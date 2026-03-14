// RUN: %empty-directory(%t)

// RUN: %target-build-swift-dylib(%t/%target-library-name(bitwise_copyable_other)) -enable-library-evolution %S/Inputs/bitwise_copyable_other.swift -emit-module -emit-module-path %t/bitwise_copyable_other.swiftmodule -module-name bitwise_copyable_other
// RUN: %target-codesign %t/%target-library-name(bitwise_copyable_other)

// RUN: %target-build-swift %s -lbitwise_copyable_other -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main

// RUN: %target-run %t/main %t/%target-library-name(bitwise_copyable_other)

// REQUIRES: executable_test

import StdlibUnittest
import bitwise_copyable_other

var bitwise = TestSuite("BitwiseCopyable")

bitwise.test("Conditional conformance with resilient struct") {
  func callee(_: consuming BitwiseStruct<LifetimeTracked>) {}

  for _ in 0..<10 {
    callee(BitwiseStruct(t: LifetimeTracked(0)))
  }
}

bitwise.test("Conditional conformance with resilient enum") {
  func callee(_: consuming BitwiseEnum<LifetimeTracked>) {}

  for _ in 0..<10 {
    callee(BitwiseEnum.t(LifetimeTracked(0)))
  }
}

runAllTests()
