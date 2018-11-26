// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -emit-module -o /dev/null %s
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
} // CHECK: {{^[}]$}}

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
} // CHECK: {{^[}]$}}

// CHECK: extension UFIProto {{[{]$}}
extension UFIProto {
  // CHECK: public func publicMethod(){{$}}
  public func publicMethod() {}
  internal func internalMethod_BAD() {}

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal func ufiMethod(){{$}}
  @usableFromInline internal func ufiMethod() {}
} // CHECK: {{^[}]$}}
