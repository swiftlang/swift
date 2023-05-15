// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PickBestCandidate -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PickBestCandidate -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PickBestCandidate.symbols.json

public protocol P {
  func foo()
  func bar()

  var baz: Int { get }
  var qux: Int { get }
}

public protocol Q : P {}
extension Q {
  public func foo() {}
  public func bar() {}

  public var baz: Int { 0 }
  public var qux: Int { 0 }
}

public protocol R : Q {}
extension R {
  public func foo() {}
  public func bar() {}

  public var baz: Int { 1 }
  public var qux: Int { 1 }
}

public struct MyStruct: R {
  public func bar() {}

  public var qux: Int { 2 }
}

// MyStruct gets one and only one synthesized `foo` and `baz`.
// MyStruct gets no synthesized `bar` and `qux`, because it has its own implementation.
// CHECK-COUNT-2: "precise": {{.*}}::SYNTHESIZED::
// CHECK-NOT:     "precise": {{.*}}::SYNTHESIZED::
