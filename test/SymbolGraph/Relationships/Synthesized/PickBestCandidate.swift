// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PickBestCandidate -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PickBestCandidate -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PickBestCandidate.symbols.json

public protocol P {
  func foo()
  func bar()
}

public protocol Q : P {}
extension Q {
  public func foo() {}
  public func bar() {}
}

public protocol R : Q {}
extension R {
  public func foo() {}
  public func bar() {}
}

public struct MyStruct: R {
  public func bar() {}
}

// MyStruct gets one and only one synthesized `foo`.
// MyStruct gets no synthesized `bar`, because it has its own implementation.
// CHECK-COUNT-1: ::SYNTHESIZED::
