// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %S/Inputs/result_builder.swift -emit-module -emit-module-path %t/result_builder.swiftmodule
// RUN: %target-swift-frontend -emit-ir %s -I %t -g -O

// FIXME: Devise a test case that does not involve -O.

import result_builder

struct S {
  static func foo(_ s: S?) -> S? {s}
  static func foo(_ s: S) -> S {s}
}

public func test() {
  View { View { S() } }
  .closure {
    let _: S? = .foo(S())
  }
}
