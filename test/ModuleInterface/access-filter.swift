// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %s -module-name AccessFilter
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
  // CHECK: @_hasInitialValue public static var secretlySettable: Swift.Int {
  // CHECK-NEXT: get
  // CHECK-NEXT: }
  public private(set) static var secretlySettable: Int = 0
} // CHECK: {{^[}]$}}

extension InternalStruct_BAD: PublicProto {
  func requirement() {}
  internal static var dummy: Int { return 0 }
}

// CHECK: extension UFIStruct : AccessFilter.PublicProto {{[{]$}}
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
  // CHECK-NEXT: case y(Swift.Int)
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
  // CHECK-NEXT: case y(Swift.Int)
  case y(Int)
} // CHECK-NEXT: {{^[}]$}}

// CHECK: public class PublicClass {{[{]$}}
public class PublicClass {
} // CHECK: {{^[}]$}}

class InternalClass_BAD {
}

// CHECK: @usableFromInline
// CHECK-NEXT: internal class UFIClass {{[{]$}}
@usableFromInline class UFIClass {
} // CHECK: {{^[}]$}}

// CHECK: public struct GenericStruct<T>
public struct GenericStruct<T> {}

// CHECK: extension GenericStruct where T == AccessFilter.PublicStruct {{[{]$}}
extension GenericStruct where T == AccessFilter.PublicStruct {
  // CHECK-NEXT: public func constrainedToPublicStruct(){{$}}
  public func constrainedToPublicStruct() {}
} // CHECK-NEXT: {{^[}]$}}
// CHECK: extension GenericStruct where T == AccessFilter.UFIStruct {{[{]$}}
extension GenericStruct where T == AccessFilter.UFIStruct {
  // CHECK-NEXT: @usableFromInline{{$}}
  // CHECK-NEXT: internal func constrainedToUFIStruct(){{$}}
  @usableFromInline internal func constrainedToUFIStruct() {}
} // CHECK-NEXT: {{^[}]$}}
extension GenericStruct where T == InternalStruct_BAD {
  @usableFromInline internal func constrainedToInternalStruct_BAD() {}
}

// CHECK: extension GenericStruct where T == AccessFilter.PublicStruct {{[{]$}}
extension GenericStruct where PublicStruct == T {
  // CHECK-NEXT: public func constrainedToPublicStruct2(){{$}}
  public func constrainedToPublicStruct2() {}
} // CHECK-NEXT: {{^[}]$}}
// CHECK: extension GenericStruct where T == AccessFilter.UFIStruct {{[{]$}}
extension GenericStruct where UFIStruct == T {
  // CHECK-NEXT: @usableFromInline{{$}}
  // CHECK-NEXT: internal func constrainedToUFIStruct2(){{$}}
  @usableFromInline internal func constrainedToUFIStruct2() {}
} // CHECK-NEXT: {{^[}]$}}
extension GenericStruct where InternalStruct_BAD == T {
  @usableFromInline internal func constrainedToInternalStruct2_BAD() {}
}

// CHECK: extension GenericStruct where T : AccessFilter.PublicProto {{[{]$}}
extension GenericStruct where T: PublicProto {
  // CHECK-NEXT: public func constrainedToPublicProto(){{$}}
  public func constrainedToPublicProto() {}
} // CHECK-NEXT: {{^[}]$}}
// CHECK: extension GenericStruct where T : AccessFilter.UFIProto {{[{]$}}
extension GenericStruct where T: UFIProto {
  // CHECK-NEXT: @usableFromInline{{$}}
  // CHECK-NEXT: internal func constrainedToUFIProto(){{$}}
  @usableFromInline internal func constrainedToUFIProto() {}
} // CHECK-NEXT: {{^[}]$}}
extension GenericStruct where T: InternalProto_BAD {
  @usableFromInline internal func constrainedToInternalProto_BAD() {}
}

// CHECK: extension GenericStruct where T : AccessFilter.PublicClass {{[{]$}}
extension GenericStruct where T: PublicClass {
  // CHECK-NEXT: public func constrainedToPublicClass(){{$}}
  public func constrainedToPublicClass() {}
} // CHECK-NEXT: {{^[}]$}}
// CHECK: extension GenericStruct where T : AccessFilter.UFIClass {{[{]$}}
extension GenericStruct where T: UFIClass {
  // CHECK-NEXT: @usableFromInline{{$}}
  // CHECK-NEXT: internal func constrainedToUFIClass(){{$}}
  @usableFromInline internal func constrainedToUFIClass() {}
} // CHECK-NEXT: {{^[}]$}}
extension GenericStruct where T: InternalClass_BAD {
  @usableFromInline internal func constrainedToInternalClass_BAD() {}
}

// CHECK: extension GenericStruct where T : AnyObject {{[{]$}}
extension GenericStruct where T: AnyObject {
  // CHECK-NEXT: public func constrainedToAnyObject(){{$}}
  public func constrainedToAnyObject() {}
} // CHECK-NEXT: {{^[}]$}}

public struct PublicAliasBase {}
internal struct ReallyInternalAliasBase_BAD {}

// CHECK: public typealias PublicAlias = AccessFilter.PublicAliasBase
public typealias PublicAlias = PublicAliasBase
internal typealias InternalAlias_BAD = PublicAliasBase
// CHECK: @usableFromInline
// CHECK-NEXT: internal typealias UFIAlias = AccessFilter.PublicAliasBase
@usableFromInline internal typealias UFIAlias = PublicAliasBase

internal typealias ReallyInternalAlias_BAD = ReallyInternalAliasBase_BAD

// CHECK: extension GenericStruct where T == AccessFilter.PublicAlias {{[{]$}}
extension GenericStruct where T == PublicAlias {
  // CHECK-NEXT: public func constrainedToPublicAlias(){{$}}
  public func constrainedToPublicAlias() {}
} // CHECK-NEXT: {{^[}]$}}
// CHECK: extension GenericStruct where T == AccessFilter.UFIAlias {{[{]$}}
extension GenericStruct where T == UFIAlias {
  // CHECK-NEXT: @usableFromInline{{$}}
  // CHECK-NEXT: internal func constrainedToUFIAlias(){{$}}
  @usableFromInline internal func constrainedToUFIAlias() {}
} // CHECK-NEXT: {{^[}]$}}
extension GenericStruct where T == InternalAlias_BAD {
  // FIXME: We could print this one by desugaring; it is indeed public.
  @usableFromInline internal func constrainedToInternalAlias() {}
}
extension GenericStruct where T == ReallyInternalAlias_BAD {
  @usableFromInline internal func constrainedToPrivateAlias() {}
}

extension GenericStruct {
  // For the next extension's test.
  public func requirement() {}
}
extension GenericStruct: PublicProto where T: InternalProto_BAD {
  @usableFromInline internal func conformance_BAD() {}
}


public struct MultiGenericStruct<First, Second> {}

// CHECK: extension MultiGenericStruct where First == AccessFilter.PublicStruct, Second == AccessFilter.PublicStruct {{[{]$}}
extension MultiGenericStruct where First == PublicStruct, Second == PublicStruct {
  // CHECK-NEXT: public func publicPublic(){{$}}
  public func publicPublic() {}
} // CHECK-NEXT: {{^[}]$}}

extension MultiGenericStruct where First == PublicStruct, Second == InternalStruct_BAD {
  @usableFromInline internal func publicInternal_BAD() {}
}
extension MultiGenericStruct where First == InternalStruct_BAD, Second == PublicStruct {
  @usableFromInline internal func internalPublic_BAD() {}
}
