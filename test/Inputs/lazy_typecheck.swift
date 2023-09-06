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

// MARK: - Nominal types

public protocol PublicProto {
  func req() -> Int // expected-note 2 {{protocol requires function 'req()' with type '() -> Int'; add a stub for conformance}}
}

protocol InternalProto {
  func goodReq() -> Int // expected-note 2 {{protocol requires function 'goodReq()' with type '() -> Int'; add a stub for conformance}}
  func badReq() -> DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

public struct PublicStruct {
  // FIXME: Test properties

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

struct InternalStruct: DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  var x: DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}}

  func f(_ x: DoesNotExist) {} // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

public class PublicClass {
  // FIXME: Test properties

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

class InternalClass: DoesNotExist { // expected-error {{cannot find type 'DoesNotExist' in scope}}
  init(x: DoesNotExist) {} // expected-error {{cannot find type 'DoesNotExist' in scope}}
}

// MARK: - Conformances

public struct PublicStructConformingToPublicProto: PublicProto {
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

extension String: PublicProto {
  public func req() -> Int {
    return true // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
  }
}

extension String: InternalProto {} // expected-error {{type 'String' does not conform to protocol 'InternalProto'}}

struct InternalStructConformingToPublicProto: PublicProto { // expected-error {{type 'InternalStructConformingToPublicProto' does not conform to protocol 'PublicProto'}}
}

extension InternalStruct: PublicProto { // expected-error {{type 'InternalStruct' does not conform to protocol 'PublicProto'}}
}

struct InternalStructConformingToInternalProto: InternalProto { // expected-error {{type 'InternalStructConformingToInternalProto' does not conform to protocol 'InternalProto'}}
}

// FIXME: Test enums
// FIXME: Test global vars
