// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name protocol_extension_type_witness
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name protocol_extension_type_witness
// RUN: %FileCheck %s < %t.swiftinterface

public protocol P {
  associatedtype A
  associatedtype B
  associatedtype C
  associatedtype D

  func b(_: B)
  func c(_: C)
  func d(_: D)
}

extension P {
  public typealias _Default_A = B
  public typealias Alias = D

  public func c(_: Alias) {}
}

public struct S<B>: P {
  public func b(_: B) {}
  public func d(_: String) {}
}

// CHECK-LABEL: public struct S<B> : protocol_extension_type_witness.P {
// CHECK-NEXT:    public func b(_: B)
// CHECK-NEXT:    public func d(_: Swift.String)
// CHECK-NEXT:    public typealias A = B
// CHECK-NEXT:    public typealias C = Swift.String
// CHECK-NEXT:    public typealias D = Swift.String
// CHECK-NEXT:  }
