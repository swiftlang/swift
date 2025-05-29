// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {}

protocol P2 {}

protocol IteratorProtocol {
  associatedtype Element

  func next() -> Element?
}

// CHECK: ExtensionDecl line={{[0-9]+}} base=LocalArray
// CHECK: Generic signature: <Element where Element : P1, Element : P2>
extension LocalArray where Element : P2 {
  static func ==(lhs: Self, rhs: Self) -> Bool {}
}

// CHECK: requirement_inference_funny_order.(file).LocalArray@
// CHECK: Generic signature: <Element where Element : P1>
struct LocalArray<Element : P1>: IteratorProtocol {
  func next() -> Element? {}
}
