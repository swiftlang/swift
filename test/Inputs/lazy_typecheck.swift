// This source file contains intentional type checking errors that should be
// avoided when compiling with -experimental-lazy-typecheck and emitting module
// outputs since all the errors occur in regions of the AST that do not
// need to be type checked in order to emit a module or module interface.

// MARK: - Global functions

public func publicFunc() -> Int {
  return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
}

public func publicFuncWithDefaultArg(_ x: Int = 1) -> Int {
  return doesNotExist() // expected-error {{cannot find 'doesNotExist' in scope}}
}

package func packageFunc() -> Int {
  return false // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
}

func internalFunc() -> DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  return 1
}

@inlinable func inlinableFunc() -> Int {
  return 1
}

private func privateFunc() -> DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  return 1
}

public func constrainedGenericPublicFunction<T>(_ t: T) where T: PublicProto {
  doesNotExist() // expected-error {{cannot find 'doesNotExist' in scope}}
}

@_specialize(exported: true, where T == PublicProto)
public func publicSpecializedFunc<T>(_ t: T) -> T {
  return doesNotExist() // expected-error {{cannot find 'doesNotExist' in scope}}
}

@available(SwiftStdlib 5.1, *)
public func publicFuncWithOpaqueReturnType() -> some PublicProto { // expected-note {{opaque return type declared here}}
  return 1 // expected-error {{return type of global function 'publicFuncWithOpaqueReturnType()' requires that 'Int' conform to 'PublicProto'}}
}

@available(SwiftStdlib 5.1, *)
@_alwaysEmitIntoClient public func publicAEICFuncWithOpaqueReturnType() -> some Any {
  if #available(macOS 20, *) {
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
      _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
    }
    set {
      _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
    }
  }

  public var projectedValue: PublicWrapper { self }

  public init(wrappedValue value: T) {
    _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
  }
}

@propertyWrapper
struct InternalWrapper<T> {} // expected-error {{property wrapper type 'InternalWrapper' does not contain a non-static property named 'wrappedValue'}}

// MARK: - Global vars

public var publicGlobalVar: Int = 0
public var publicGlobalVarInferredType = ""
public var (publicGlobalVarInferredTuplePatX, publicGlobalVarInferredTuplePatY) = (0, 1)

var internalGlobalVar: DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}
var internalGlobalVarInferredType = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}

// MARK: - Nominal types

public protocol EmptyPublicProto {}

public protocol PublicProto {
  func req() -> Int // expected-note 2 {{protocol requires function 'req()' with type '() -> Int'; add a stub for conformance}}
}

@rethrows public protocol PublicRethrowsProto {
  func req() throws -> Int
}

protocol InternalProto {
  func goodReq() -> Int // expected-note 2 {{protocol requires function 'goodReq()' with type '() -> Int'; add a stub for conformance}}
  func badReq() -> DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

protocol InternalProtoConformingToPublicProto: PublicProto {
  func internalReq() -> DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

public struct PublicStruct {
  public var publicProperty: Int
  public var publicPropertyInferredType = ""
  @PublicWrapper public var publicWrappedProperty = 3.14

  public init(x: Int) {
    _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
  }

  public func publicMethod() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }

  public static func publicStaticMethod() {
    _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
  }

  func internalMethod() -> DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
    return 1
  }

  static func internalStaticMethod() -> DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
    return 1
  }
}

public struct PublicGenericStruct<T> {
  public func publicMethod() -> T {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'T'}}
  }
}

struct InternalStruct: DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  var x: DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}

  func f(_ x: DoesNotExist) {} // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

public class PublicClass {
  public var publicProperty: Int
  public var publicPropertyInferredType = ""

  public init(x: Int) {
    _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
  }

  // FIXME: TBDGen causes this constructor to be type checked
//  init(_ x: DoesNotExist) {}

  public func publicMethod() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }

  public class func publicClassMethod() {
    _ = DoesNotExist() // expected-error {{cannot find 'DoesNotExist' in scope}}
  }

  // FIXME: TBDGen causes these methods to be type checked
//  func internalMethod() -> DoesNotExist {}
//  class func internalClassMethod() -> DoesNotExist {}
}

public class PublicDerivedClass: PublicClass {}

class InternalClass: DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  init(x: DoesNotExist) {} // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

public enum PublicEnum {
  case a
  case b(x: Int)

  public func publicMethod() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }

  public var publicComputedVar: Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

enum InternalEnum {
  case bad(DoesNotExist) // expected-error {{cannot find type 'DoesNotExist' in scope}}

  func method() -> DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  }
}

// MARK: - Conformances

public struct PublicStructConformingToPublicProto: PublicProto {
  public init() {}
  public func req() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

public struct PublicStructIndirectlyConformingToPublicProto: InternalProtoConformingToPublicProto {
  public init() {}
  public func req() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

public class PublicClassConformingToPublicProto: PublicProto {
  public init() {}
  public func req() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

public class PublicClassInheritingConformanceToPublicProto: PublicClassConformingToPublicProto {}

extension String: PublicProto {
  public func req() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

extension String: InternalProto {} // expected-error {{type 'String' does not conform to protocol 'InternalProto'}}

extension Int: PublicRethrowsProto {
  public func req() throws -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

struct InternalStructConformingToPublicProto: PublicProto { // expected-error {{type 'InternalStructConformingToPublicProto' does not conform to protocol 'PublicProto'}}
}

extension InternalStruct: PublicProto { // expected-error {{type 'InternalStruct' does not conform to protocol 'PublicProto'}}
}

struct InternalStructConformingToInternalProto: InternalProto { // expected-error {{type 'InternalStructConformingToInternalProto' does not conform to protocol 'InternalProto'}}
}

struct InternalStructForConstraint {}

extension PublicGenericStruct where T == InternalStructForConstraint {}

extension PublicGenericStruct: EmptyPublicProto where T == InternalStructForConstraint {}

// MARK: - Type aliases

public typealias PublicStructAlias = PublicStruct
typealias InternalTypeAlias = DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}

// MARK: - Compiler directives

extension PublicStruct {
#if FLAG
  public static func activeMethod() {}
#else
  public static func inactiveMethod() -> DoesNotExist {}
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
