
struct NoTypecheck {
  static let int: Int = 0
  static func fatalError() -> Never { Swift.fatalError() }
}

protocol NoTypecheckProto {}

extension NoTypecheck: PublicProto {
  func req() -> Int { return 0 }
}

// MARK: - Global functions

public func publicFunc() -> Int {
  return NoTypecheck.int
}

public func publicFuncReturnsTypealias() -> PublicIntAlias {
  return NoTypecheck.int
}

public func publicFuncWithDefaultArg(_ x: Int = 1) -> Int {
  return NoTypecheck.int
}

@inlinable public func publicInlinableFunc() -> Int {
  lazy var x = inlinableFunc()
  func nestedFunc() {}
  defer { nestedFunc() }
  return x
}

package func packageFunc() -> Int {
  return NoTypecheck.int
}

func internalFunc() -> NoTypecheck {
  return NoTypecheck()
}

@inlinable func inlinableFunc() -> Int {
  return 1
}

private func privateFunc() -> NoTypecheck {
  return NoTypecheck()
}

public func constrainedGenericPublicFunction<T>(_ t: T) where T: PublicProto {
  _ = NoTypecheck()
}

@_specialize(exported: true, where T == PublicProto)
public func publicSpecializedFunc<T>(_ t: T) -> T {
  _ = NoTypecheck()
  return t
}

@available(SwiftStdlib 5.1, *)
public func publicFuncWithOpaqueReturnType() -> some PublicProto {
  return NoTypecheck()
}

@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient public func publicAEICFuncWithOpaqueReturnType() -> some Any {
  if #available(macOS 99, *) {
    return 3
  } else {
    return "hi"
  }
}

// MARK: - Property wrappers

@propertyWrapper
public struct PublicWrapper<T> {
  public var wrappedValue: T {
    get {
      NoTypecheck.fatalError()
    }
    set {
      NoTypecheck.fatalError()
    }
  }

  public var projectedValue: PublicWrapper { self }

  public init(wrappedValue value: T) {
    NoTypecheck.fatalError()
  }
}

@propertyWrapper
struct InternalWrapper {
  var wrappedValue: NoTypecheck
}

// MARK: - Global vars

public var publicGlobalVar: Int = NoTypecheck.int
public var publicGlobalVarTypealias: PublicIntAlias = 1
public var publicGlobalVarInferredType = ""
public var publicGlobalVarInferredInferredGeneric: [_] = [1]
public var publicGlobalVarTypealiasGeneric: PublicIntAlias? = 1
public var (publicGlobalVarInferredTuplePatX, publicGlobalVarInferredTuplePatY) = (0, 1)

var internalGlobalVar: NoTypecheck = NoTypecheck()
var internalGlobalVarInferredType = NoTypecheck()
var internalGlobalTypealiasVar: PublicIntAlias = NoTypecheck.int

@backDeployed(before: macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99)
public private(set) var publicGlobalVarBackDeployedWithPrivateSetter: Int {
  get { 0 }
  set { // Implicitly not @backDeployed.
    _ = NoTypecheck()
  }
}

@backDeployed(before: macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99)
public internal(set) var publicGlobalVarBackDeployedWithInternalSetter: Int {
  get { 0 }
  set { // Implicitly not @backDeployed.
    _ = NoTypecheck()
  }
}

@backDeployed(before: macOS 99, iOS 99, tvOS 99, watchOS 99, visionOS 99)
public package(set) var publicGlobalVarBackDeployedWithPackageSetter: Int {
  get { 0 }
  set { // Implicitly not @backDeployed.
    _ = NoTypecheck()
  }
}

// MARK: - Nominal types

public protocol EmptyPublicProto {}

public protocol PublicProto {
  func req() -> Int
}

public protocol PublicProtoWithAssociatedType {
  associatedtype A
  func req() -> A
}

@rethrows public protocol PublicRethrowsProto {
  func req() throws -> Int
}

@MainActor public protocol MainActorProtocol {
  func req() throws -> Int
}

extension MainActorProtocol {
  public func req() throws -> Int {
    return 1
  }
}

protocol InternalProtoWithAssociatedType {
  associatedtype A
  func internalReq() -> A
}

protocol InternalProtoConformingToPublicProto: PublicProto {
  func internalReq() -> NoTypecheck
}

public struct PublicStruct {
  public var publicProperty: Int = NoTypecheck.int
  public var publicTypealiasProperty: PublicIntAlias = 1
  public var publicPropertyInferredType = ""
  public var publicLazyProperty: Int = NoTypecheck.int
  public var publicLazyPropertyInferred = 1
  @PublicWrapper public var publicWrappedProperty = 3.14
  @_transparent public var publicTransparentProperty: Int {
    get { return 1 }
  }
  public dynamic var publicDynamicProperty: Int = 5
  public static let publicStaticProperty: Int = NoTypecheck.int
  public static let publicStaticPropertyInferred = 2

  public init(x: Int) {
    _ = NoTypecheck()
  }

  public func publicMethod() -> Int {
    return NoTypecheck.int
  }

  @MainActor public func publicMainActorMethod() -> Int {
    return NoTypecheck.int
  }

  public static func publicStaticMethod() {
    _ = NoTypecheck()
  }

  func internalMethod() -> NoTypecheck {
    return NoTypecheck()
  }

  static func internalStaticMethod() -> NoTypecheck {
    return NoTypecheck()
  }
}

public struct PublicGenericStruct<T> {
  var t: T

  public func publicMethod() -> T {
    _ = NoTypecheck()
    return t
  }
}

@frozen public struct FrozenPublicStruct {
  private(set) var varWithPrivateSetter: Int = 1

  public init(_ varWithPrivateSetter: Int) {
    self.varWithPrivateSetter = varWithPrivateSetter
  }
}

struct InternalStruct: NoTypecheckProto {
  var x: NoTypecheck

  func f(_ x: NoTypecheck) {}
}

public class PublicClass {
  public var publicProperty: Int = NoTypecheck.int
  public var publicPropertyInferredType = ""
  public var publicLazyProperty: Int = NoTypecheck.int
  public var publicLazyPropertyInferred = 1
  @PublicWrapper public final var publicFinalWrappedProperty: Bool = false
  public static let publicStaticProperty: Int = NoTypecheck.int
  public static let publicStaticPropertyInferred = 2

  public init(x: Int) {
    self.publicProperty = x
    _ = NoTypecheck()
  }

  convenience init(_ x: NoTypecheck) {
    NoTypecheck.fatalError()
  }

  public func publicMethod() -> Int {
    return NoTypecheck.int
  }

  public class func publicClassMethod() {
    _ = NoTypecheck()
  }

  public static func publicStaticMethod() {
    _ = NoTypecheck()
  }

  func internalMethod() -> NoTypecheck {
    return NoTypecheck()
  }

  class func internalClassMethod() -> NoTypecheck {
    return NoTypecheck()
  }
}

public class PublicDerivedClass: PublicClass {}

open class PublicClassSynthesizedDesignatedInit {}

class InternalClass: NoTypecheckProto {
  init(x: NoTypecheck) {}
}

public enum PublicEnum {
  case a
  case b(x: Int)

  public func publicMethod() -> Int {
    return NoTypecheck.int
  }

  public var publicComputedVar: Int {
    return NoTypecheck.int
  }
}

enum InternalEnum {
  case bad(NoTypecheck)

  func method() -> NoTypecheck {
    return NoTypecheck()
  }
}

// MARK: - Conformances

public struct PublicStructConformingToPublicProto: PublicProto {
  public init() {}
  public func req() -> Int {
    return NoTypecheck.int
  }
}

public struct PublicStructIndirectlyConformingToPublicProto: InternalProtoConformingToPublicProto {
  public init() {}
  public func req() -> Int {
    return NoTypecheck.int
  }

  func internalReq() -> NoTypecheck {
    return NoTypecheck()
  }
}

public class PublicClassConformingToPublicProto: PublicProto {
  public init() {}
  public func req() -> Int {
    return NoTypecheck.int
  }
}

public class PublicClassInheritingConformanceToPublicProto: PublicClassConformingToPublicProto {}

extension String: PublicProto {
  public func req() -> Int {
    return NoTypecheck.int
  }
}

extension String: InternalProtoWithAssociatedType {
  func internalReq() -> NoTypecheck {
    return NoTypecheck()
  }
}

extension Int: PublicRethrowsProto {
  public func req() throws -> Int {
    return NoTypecheck.int
  }
}

struct InternalStructConformingToPublicProto: PublicProtoWithAssociatedType {
  typealias A = NoTypecheck
  func req() -> A {
    return NoTypecheck()
  }
}

extension InternalStruct: PublicProtoWithAssociatedType {
  typealias A = NoTypecheck
  func req() -> A {
    return NoTypecheck()
  }
}

struct InternalStructConformingToInternalProto: InternalProtoWithAssociatedType {
  func internalReq() -> NoTypecheck {
    return NoTypecheck()
  }
}

struct InternalStructForConstraint {}

extension PublicGenericStruct where T == InternalStructForConstraint {}

extension PublicGenericStruct: EmptyPublicProto where T == InternalStructForConstraint {}

// MARK: - Type aliases

public typealias PublicIntAlias = Int
public typealias PublicStructAlias = PublicStruct
typealias InternalTypeAlias = NoTypecheck

// MARK: - Compiler directives

extension PublicStruct {
#if FLAG
  public static func activeMethod() {}
#else
  public static func inactiveMethod() -> NoTypecheck {}
#endif
}

// MARK: - Operators & Precedence Groups

precedencegroup FooPrecedence {
  assignment: true
  associativity: right
}

infix operator <<<: FooPrecedence

extension PublicStruct {
  public static func <<<(lhs: inout Self, rhs: Self) {
    lhs = rhs
  }
}
