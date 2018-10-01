// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.swiftinterface

// CHECK-LABEL: public protocol SimpleProto {
public protocol SimpleProto {
  // CHECK: associatedtype Element
  associatedtype Element
  // CHECK: associatedtype Inferred
  associatedtype Inferred
  func inference(_: Inferred)
} // CHECK: {{^}$}}

// CHECK-LABEL: public struct SimplImpl<Element> : SimpleProto {
public struct SimplImpl<Element>: SimpleProto {
  // NEGATIVE-NOT: typealias Element =
  // CHECK: public func inference(_: Int){{$}}
  public func inference(_: Int) {}
  // CHECK: public typealias Inferred = Swift.Int
} // CHECK: {{^}$}}
