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
  func req() -> Int
}

protocol InternalProto {
  // FIXME: Serialization causes typechecking of protocols regardless of access level
//  func req() -> DoesNotExist
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
// FIXME: Test enums
// FIXME: Test conformances
// FIXME: Test global vars
