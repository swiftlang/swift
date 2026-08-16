// Matching rules for `@cxx @implementation` functions declared in Swift
// extensions of imported C++ namespaces.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Namespaces


// Members of a namespace, a nested namespace, a rename onto a namespace
// member, and arity overloads all match with no diagnostics.

extension Outer {
  @cxx @implementation
  static func add(_ a: Int32, _ b: Int32) -> Int32 { return a + b }

  @cxx(renamedTarget) @implementation
  static func swiftRenamed(_ x: Int32) -> Int32 { return x * 2 }

  @cxx @implementation
  static func overloadedByArity(_ x: Int32) -> Int32 { return x + 1 }

  @cxx @implementation
  static func overloadedByArity(_ x: Int32, _ y: Int32) -> Int32 { return x + y }
}

extension Outer.Inner {
  @cxx @implementation
  static func nestedFunc(_ x: Int32) -> Int32 { return x }
}


// Name not declared in the namespace

extension Outer {
  // expected-error@+1{{could not find imported function 'notDeclared' matching static method 'notDeclared'; make sure you import the module or header that declares it}}
  @cxx @implementation
  static func notDeclared(_ x: Int32) -> Int32 { return x }
}


// Inline

extension Outer {
  // expected-error@+2{{static method 'inlineFunc' cannot implement C++ function 'inlineFunc' because it already has a definition}}
  @cxx @implementation
  static func inlineFunc(_ x: Int32) -> Int32 { return x }
}


// Same-arity overloads are told apart by parameter type, as at the top level.

extension Outer {
  @cxx @implementation
  static func sameArityOverload(_ x: Int32) -> Int32 { return x }

  @cxx @implementation
  static func sameArityOverload(_ x: Double) -> Double { return x }
}


// An instance method cannot implement a namespace member (a free C++ function
// imports as a static member of the namespace enum).

extension Outer {
  // expected-error@+2{{instance method 'instanceMismatch' does not match static method declared in header}}
  @cxx @implementation
  func instanceMismatch(_ x: Int32) -> Int32 { return x }
}


// A clang enum that is not a namespace is still a rejected type context.

extension PlainEnum {
  // expected-error@+2{{@cxx can only be applied to global functions or functions in extensions of C++ namespaces}}
  // expected-error@+1{{could not find imported function 'notANamespaceMember' matching static method 'notANamespaceMember'; make sure you import the module or header that declares it}}
  @cxx @implementation
  static func notANamespaceMember(_ x: Int32) -> Int32 { return x }
}
