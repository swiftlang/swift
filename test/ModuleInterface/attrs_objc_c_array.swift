// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -emit-module %t/AttrsObjCCArray.swift -o %t \
// RUN:   -module-name AttrsObjCCArray \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -enable-experimental-feature ObjCAttrAllowsCArrays \
// RUN:   -emit-module-interface-path %t/AttrsObjCCArray.swiftinterface

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t \
// RUN:   -typecheck-module-from-interface %t/AttrsObjCCArray.swiftinterface

// RUN: %FileCheck %s --input-file %t/AttrsObjCCArray.swiftinterface \
// RUN:   --implicit-check-not='$ObjCAttrAllowsCArrays'

// Make sure it compiles if everything is `#if`'d out.
// RUN: %empty-directory(%t/nofeature)
// RUN: sed -e 's/ -enable-experimental-feature ObjCAttrAllowsCArrays//' \
// RUN:   %t/AttrsObjCCArray.swiftinterface \
// RUN:   > %t/nofeature/AttrsObjCCArray.swiftinterface
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) \
// RUN:   -compile-module-from-interface \
// RUN:   %t/nofeature/AttrsObjCCArray.swiftinterface \
// RUN:   -o %t/nofeature/AttrsObjCCArray.swiftmodule

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t \
// RUN:   -emit-module %t/Dependent.swift -o %t -module-name Dependent \
// RUN:   -swift-version 6 -enable-library-evolution \
// RUN:   -enable-experimental-feature ObjCAttrAllowsCArrays \
// RUN:   -emit-module-interface-path %t/Dependent.swiftinterface
// RUN: %FileCheck %s --check-prefix=DEPENDENT \
// RUN:   --input-file %t/Dependent.swiftinterface \
// RUN:   --implicit-check-not='$ObjCAttrAllowsCArrays'
// RUN: sed -e 's/ -enable-experimental-feature ObjCAttrAllowsCArrays//' \
// RUN:   %t/Dependent.swiftinterface > %t/nofeature/Dependent.swiftinterface
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %t/nofeature \
// RUN:   -compile-module-from-interface \
// RUN:   %t/nofeature/Dependent.swiftinterface \
// RUN:   -o %t/nofeature/Dependent.swiftmodule

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCAttrAllowsCArrays

//--- AttrsObjCCArray.swift

import Foundation

//
// @objc and @c declarations (even implicit) with C array types are dropped.
//

public class CArrayMembers: NSObject {
  @objc public func array(_ p: UnsafeMutablePointer<((Int32, Int32), (Int32, Int32))>) {}
  // CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
  // CHECK-NEXT: @objc public func array
  // CHECK-NEXT: #endif

  @objc public func plain(_ p: UnsafeMutablePointer<Int32>) {}
  // CHECK-NEXT: @objc public func plain
  public func notObjC(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  // CHECK-NEXT: public func notObjC

  @objc public var stored: UnsafeMutablePointer<(Int32, Int32)>?
  // CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
  // CHECK-NEXT: @objc public var stored
  // CHECK-NEXT: #endif
}

@objcMembers public class InferredMembers: NSObject {
  public func inferred(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  // CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
  // CHECK-NEXT: @objc public func inferred
  // CHECK-NEXT: #endif
}

extension InferredMembers {
  @objc public func inExtension(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  // CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
  // CHECK-NEXT: @objc dynamic public func inExtension
  // CHECK-NEXT: #endif
}

public class InferredOverride: InferredMembers {
  public override func inferred(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  // CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
  // CHECK-NEXT: @objc override public func inferred
  // CHECK-NEXT: #endif
}

@objc public func globalFunction(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc public func globalFunction
// CHECK-NEXT: #endif

//
// Types broken by dropping a member are dropped.
//

@objc public protocol CArrayProtocol {
  func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>)
  func plain()
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc public protocol CArrayProtocol {
// CHECK: #endif

@objc public protocol UnrelatedProtocol { func other() }
// CHECK: @objc public protocol UnrelatedProtocol

@frozen public struct FrozenStruct {
  public var property: CArrayProtocol?
  public var number: Int
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public struct FrozenStruct

public struct ResilientStruct {
  public var property: CArrayProtocol?
  public var number: Int
}
// CHECK: public struct ResilientStruct {
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var property
// CHECK-NEXT: #endif
// CHECK-NEXT: public var number

@frozen public enum FrozenEnum {
  case value(CArrayProtocol)
  case none
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public enum FrozenEnum

public enum ResilientEnum {
  case value(CArrayProtocol)
  case none
}
// CHECK: public enum ResilientEnum {
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: case value
// CHECK-NEXT: #endif
// CHECK-NEXT: case none

// So are declarations that refer to a dropped type.

public func freeFunction(_ p: any CArrayProtocol) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func freeFunction

public func takesFrozen(_ f: FrozenStruct) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func takesFrozen

@objc public protocol RefiningProtocol: CArrayProtocol { func more() }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc public protocol RefiningProtocol

public class CArrayWitness: NSObject, CArrayProtocol {
  public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  public func plain() {}
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class CArrayWitness

public class CArraySubclass: CArrayWitness {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class CArraySubclass

public class Users: NSObject {
  public var property: CArrayProtocol?
  public func parameter(_ p: CArrayProtocol) {}
  public func generic<T: CArrayProtocol>(_ t: T) {}
  public func unrelated(_ u: UnrelatedProtocol) {}
}
// CHECK: public class Users
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var property
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func parameter
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func generic
// CHECK-NEXT: #endif
// CHECK-NEXT: public func unrelated

public typealias ProtocolAlias = CArrayProtocol
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias ProtocolAlias

extension CArrayWitness { public func extra() {} }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}CArrayWitness

extension Users: CArrayProtocol {
  public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  public func plain() {}
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}Users : {{.*}}CArrayProtocol

extension CArrayProtocol { public func helper() {} }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}CArrayProtocol

@frozen public struct FrozenPlain { public var number: Int }
// CHECK: @frozen public struct FrozenPlain

@frozen public struct FrozenLazy {
  public init() {}
  public lazy var property: CArrayProtocol? = nil
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public struct FrozenLazy

@frozen public struct FrozenPrivate {
  public init() {}
  private var hidden: CArrayProtocol? = nil
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public struct FrozenPrivate

public struct ResilientLazy {
  public init() {}
  public lazy var property: CArrayProtocol? = nil
}
// CHECK: public struct ResilientLazy {
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var property

public struct Constrained<Element: CArrayProtocol> {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public struct Constrained

public struct WhereOnType<Element> where Element: CArrayProtocol {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public struct WhereOnType

public typealias GenericAlias<Element: CArrayProtocol> = [Element]
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias GenericAlias

public func constrainedFunction<Element: CArrayProtocol>(_ e: Element) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func constrainedFunction

public func whereFunction<Element>(_ e: Element) where Element: CArrayProtocol {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func whereFunction

public protocol AssociatedProtocol { associatedtype Element: CArrayProtocol }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public protocol AssociatedProtocol

public struct Tuples {
  public init() {}
  public var (leading, trailing): (Int, CArrayProtocol?) = (0, nil)
  public var (reversed, other): (CArrayProtocol?, Int) = (nil, 0)
  public var plain: Int = 0, droppedSecond: CArrayProtocol? = nil
  public var (fine, alsoFine): (Int, Int) = (0, 0)
}
// CHECK: public struct Tuples {
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var (leading, trailing)
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var (reversed, other)
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var plain
// CHECK-NEXT: #endif
// CHECK-NEXT: public var (fine, alsoFine)

public var (globalLeading, globalTrailing): (Int, CArrayProtocol?) = (0, nil)
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var (globalLeading, globalTrailing)

public struct Conditional<Element> { public init() {} }
// CHECK: public struct Conditional

extension Conditional where Element: CArrayProtocol { public func protocolWhere() {} }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}Conditional where Element : {{.*}}CArrayProtocol

extension Conditional where Element: Equatable { public func equatableWhere() {} }
// CHECK: extension {{.*}}Conditional where Element : {{.*}}Equatable

extension Array where Element: CArrayProtocol { public func arrayWhere() {} }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}Array where Element : {{.*}}CArrayProtocol

extension Array where Element: Equatable { public func equatableArrayWhere() {} }
// CHECK: extension {{.*}}Array where Element : {{.*}}Equatable

public struct KeptOuter {
  public class NestedWitness: NSObject, CArrayProtocol {
    public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
    public func plain() {}
  }
  public struct NestedPlain {}
}
// CHECK: public struct KeptOuter {
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class NestedWitness
// CHECK: #endif
// CHECK-NEXT: public struct NestedPlain

public class DroppedOuter: NSObject, CArrayProtocol {
  public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  public func plain() {}
  public struct Inner {}
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class DroppedOuter

extension DroppedOuter.Inner { public struct Deeper {} }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}DroppedOuter.{{.*}}Inner

public struct Outer { public struct Middle {} }
// CHECK: public struct Outer

extension Outer.Middle {
  public class Deep: NSObject, CArrayProtocol {
    public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
    public func plain() {}
  }
  public struct Fine {}
}
// CHECK: extension {{.*}}Outer.{{.*}}Middle {
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class Deep
// CHECK: #endif
// CHECK-NEXT: public struct Fine

public typealias OptionalAlias = CArrayProtocol?
public typealias ArrayAlias = [CArrayProtocol]
public typealias TupleAlias = (CArrayProtocol, Int)
public typealias FunctionAlias = (CArrayProtocol) -> Void
public typealias PlainOptionalAlias = Int?
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias OptionalAlias
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias ArrayAlias
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias TupleAlias
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public typealias FunctionAlias
// CHECK: public typealias PlainOptionalAlias

public func takesOptionalAlias(_ a: OptionalAlias) {}
public func takesFunctionAlias(_ f: FunctionAlias) {}
public func takesPlainOptionalAlias(_ a: PlainOptionalAlias) {}
@frozen public struct FrozenHoldsAlias { public var a: OptionalAlias }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func takesOptionalAlias
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func takesFunctionAlias
// CHECK: public func takesPlainOptionalAlias
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public struct FrozenHoldsAlias

public func opaqueResult() -> some CArrayProtocol { CArrayWitness() }
public func opaqueSequence() -> some Sequence<any CArrayProtocol> {
  [any CArrayProtocol]()
}
public func opaquePlain() -> some Sequence<Int> { [Int]() }
public var opaqueProperty: some CArrayProtocol { CArrayWitness() }
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func opaqueResult
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func opaqueSequence
// CHECK: public func opaquePlain
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public var opaqueProperty

public protocol AssociatedDefault { associatedtype Element = any CArrayProtocol }
public protocol AssociatedWhere { associatedtype Element where Element: CArrayProtocol }
public protocol AssociatedSameType {
  associatedtype Sequence: Swift.Sequence where Sequence.Element == any CArrayProtocol
}
public protocol AssociatedPlain {
  associatedtype Element = Int
  associatedtype Other where Other: Equatable
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public protocol AssociatedDefault
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public protocol AssociatedWhere
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public protocol AssociatedSameType
// CHECK: public protocol AssociatedPlain

@propertyWrapper @frozen public struct WrapsDropped<Value> {
  public var wrappedValue: Value
  public var extra: CArrayProtocol? = nil
  public init(wrappedValue: Value) { self.wrappedValue = wrappedValue }
}
@propertyWrapper public struct WrapsPlain<Value> {
  public var wrappedValue: Value
  public init(wrappedValue: Value) { self.wrappedValue = wrappedValue }
}
@resultBuilder @frozen public struct BuildsDropped {
  public var extra: CArrayProtocol? = nil
  public static func buildBlock(_ x: Int) -> Int { x }
}
@resultBuilder public struct BuildsPlain {
  public static func buildBlock(_ x: Int) -> Int { x }
}
public struct UsesWrappers {
  public init() {}
  @WrapsDropped public var dropped: Int = 0
  @WrapsPlain public var plain: Int = 0
}
public func builtDropped(@BuildsDropped _ body: () -> Int) {}
public func builtPlain(@BuildsPlain _ body: () -> Int) {}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @propertyWrapper @frozen public struct WrapsDropped
// CHECK: @propertyWrapper public struct WrapsPlain
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @_functionBuilder @frozen public struct BuildsDropped
// CHECK: @_functionBuilder public struct BuildsPlain
// CHECK: public struct UsesWrappers {
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: {{.*}}public var dropped
// CHECK: #endif
// CHECK-NEXT: {{.*}}public var plain
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: public func builtDropped
// CHECK: public func builtPlain

// A prespecialization declaration is printed even though it isn't visible
// outside the module, so it's dropped when its signature mentions a dropped
// type.

public class Prespecialized<Element> {
  @inlinable func target(_ p: CArrayProtocol) {}
  @_specialize(exported: true, kind: full, target: target(_:), where Element == Int)
  func __specialize_target(_ p: CArrayProtocol) { fatalError() }

  @inlinable func plainTarget(_ i: Int) {}
  @_specialize(exported: true, kind: full, target: plainTarget(_:), where Element == Int)
  func __specialize_plainTarget(_ i: Int) { fatalError() }
}
// CHECK: public class Prespecialized
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @inlinable internal func target
// CHECK-NEXT: #endif
// CHECK-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @_specialize{{.*}}target: target(_:)
// CHECK-NEXT: internal func __specialize_target
// CHECK-NEXT: #endif
// CHECK-NEXT: @inlinable internal func plainTarget
// CHECK-NEXT: @_specialize{{.*}}target: plainTarget(_:)
// CHECK-NEXT: internal func __specialize_plainTarget

// A cycle of frozen types is dropped as a whole if any of its members reaches
// a dropped type, whichever member is printed first, and kept otherwise.

@frozen public indirect enum CycleA {
  case next(CycleB)
  case end
}
@frozen public indirect enum CycleB {
  case next(CycleA)
  case leaf(CArrayProtocol)
}
@frozen public struct UsesCycle {
  public var a: CycleA
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen indirect public enum CycleA
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen indirect public enum CycleB
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen public struct UsesCycle

@frozen public indirect enum ReverseCycleB {
  case next(ReverseCycleA)
  case leaf(CArrayProtocol)
}
@frozen public indirect enum ReverseCycleA {
  case next(ReverseCycleB)
  case end
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen indirect public enum ReverseCycleB
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @frozen indirect public enum ReverseCycleA

@frozen public indirect enum SeedlessA {
  case next(SeedlessB)
  case end
}
@frozen public indirect enum SeedlessB {
  case next(SeedlessA)
  case end
}
@frozen public struct UsesSeedless {
  public var a: SeedlessA
}
// CHECK: @frozen indirect public enum SeedlessA
// CHECK: @frozen indirect public enum SeedlessB
// CHECK: @frozen public struct UsesSeedless

//
// The synthesized extensions for a transitive conformance through an internal
// protocol are dropped too.
//

@objc protocol InternalRefining: CArrayProtocol {}
public class ConformsThroughInternal: NSObject, InternalRefining {
  public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  public func plain() {}
}
// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: @objc @_inheritsConvenienceInitializers public class ConformsThroughInternal

// CHECK: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// CHECK-NEXT: extension {{.*}}ConformsThroughInternal : {{.*}}CArrayProtocol {}
// CHECK-NEXT: #endif

//--- Dependent.swift

//
// Dropping logic needs to apply across modules.
//

import AttrsObjCCArray
import Foundation

public func takesProtocol(_ p: CArrayProtocol) {}
// DEPENDENT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// DEPENDENT-NEXT: public func takesProtocol
// DEPENDENT-NEXT: #endif

public struct HoldsProtocol {
  public init() {}
  public var property: CArrayProtocol?
}
// DEPENDENT: public struct HoldsProtocol {
// DEPENDENT-NEXT: public init()
// DEPENDENT-NEXT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// DEPENDENT-NEXT: public var property

public class ConformsAcross: NSObject, CArrayProtocol {
  public func requirement(_ p: UnsafeMutablePointer<(Int32, Int32)>) {}
  public func plain() {}
}
// DEPENDENT: #if compiler(>=5.3) && $ObjCAttrAllowsCArrays
// DEPENDENT-NEXT: @objc @_inheritsConvenienceInitializers public class ConformsAcross

public func plainFunction(_ u: UnrelatedProtocol) {}
// DEPENDENT: public func plainFunction
