// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.swiftinterface

// NEGATIVE-NOT: BAD

// CHECK: public func publicFn(){{$}}
public func publicFn() {}
internal func internalFn_BAD() {}
private func privateFn_BAD() {}

// CHECK: @usableFromInline
// CHECK-NEXT: internal func ufiFn(){{$}}
@usableFromInline internal func ufiFn() {}


// CHECK: public struct PublicStruct {{[{]$}}
public struct PublicStruct {
  // CHECK: public func publicMethod(){{$}}
  public func publicMethod() {}
  internal func internalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func ufiMethod(){{$}}
  @usableFromInline internal func ufiMethod() {}
} // CHECK: {{^[}]$}}

internal struct InternalStruct_BAD {
  public func publicMethod_BAD() {}
  internal func internalMethod_BAD() {}
  @usableFromInline internal func ufiMethod_BAD() {}
}

// CHECK: @usableFromInline
// CHECK-NEXT: internal struct UFIStruct {{[{]$}}
@usableFromInline
internal struct UFIStruct {
  // FIXME: Arguably this should be downgraded to "@usableFromInline internal".
  // CHECK: public func publicMethod(){{$}}
  public func publicMethod() {}
  internal func internalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func ufiMethod(){{$}}
  @usableFromInline internal func ufiMethod() {}
} // CHECK: {{^[}]$}}

// CHECK: public protocol PublicProto {{[{]$}}
public protocol PublicProto {
  // CHECK-NEXT: associatedtype Assoc = Swift.Int
  associatedtype Assoc = Int
  // CHECK-NEXT: func requirement()
  func requirement()
} // CHECK-NEXT: {{^[}]$}}

// CHECK: extension PublicProto {{[{]$}}
extension PublicProto {
  // CHECK: public func publicMethod(){{$}}
  public func publicMethod() {}
  internal func internalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func ufiMethod(){{$}}
  @usableFromInline internal func ufiMethod() {}
} // CHECK: {{^[}]$}}

// CHECK: {{^}}extension PublicProto {{[{]$}}
public extension PublicProto {
  // CHECK: public func publicExtPublicMethod(){{$}}
  func publicExtPublicMethod() {}
  internal func publicExtInternalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func publicExtUFIMethod(){{$}}
  @usableFromInline internal func publicExtUFIMethod() {}
}

internal protocol InternalProto_BAD {
  associatedtype AssocBAD = Int
  func requirementBAD()
}

extension InternalProto_BAD {
  public func publicMethod_BAD() {}
  internal func internalMethod_BAD() {}
  @usableFromInline internal func ufiMethod_BAD() {}
}

// CHECK: @usableFromInline
// CHECK-NEXT: internal protocol UFIProto {{[{]$}}
@usableFromInline
internal protocol UFIProto {
  // CHECK-NEXT: associatedtype Assoc = Swift.Int
  associatedtype Assoc = Int
  // CHECK-NEXT: func requirement()
  func requirement()
} // CHECK-NEXT: {{^[}]$}}

// CHECK: extension UFIProto {{[{]$}}
extension UFIProto {
  // CHECK: public func publicMethod(){{$}}
  public func publicMethod() {}
  internal func internalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func ufiMethod(){{$}}
  @usableFromInline internal func ufiMethod() {}
} // CHECK: {{^[}]$}}

// CHECK: extension PublicStruct {{[{]$}}
extension PublicStruct {
  // CHECK: public private(set) static var secretlySettable: Int{{$}}
  public private(set) static var secretlySettable: Int = 0
} // CHECK: {{^[}]$}}

extension InternalStruct_BAD: PublicProto {
  func requirement() {}
  internal static var dummy: Int { return 0 }
}

// CHECK: extension UFIStruct : PublicProto {{[{]$}}
extension UFIStruct: PublicProto {
  // CHECK-NEXT: @usableFromInline
  // CHECK-NEXT: internal typealias Assoc = Swift.Int

  // FIXME: Is it okay for this non-@usableFromInline implementation to satisfy
  // the protocol?
  func requirement() {}
  internal static var dummy: Int { return 0 }
} // CHECK-NEXT: {{^[}]$}}

// CHECK: public enum PublicEnum {{[{]$}}
public enum PublicEnum {
  // CHECK-NEXT: case x
  case x
  // CHECK-NEXT: case y(Int)
  case y(Int)
} // CHECK-NEXT: {{^[}]$}}

enum InternalEnum_BAD {
  case xBAD
}

// CHECK: @usableFromInline
// CHECK-NEXT: internal enum UFIEnum {{[{]$}}
@usableFromInline enum UFIEnum {
  // CHECK-NEXT: case x
  case x
  // CHECK-NEXT: case y(Int)
  case y(Int)
} // CHECK-NEXT: {{^[}]$}}
