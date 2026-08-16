// Overload selection: which member of an imported C++ overload set a
// `@cxx @implementation` function implements. Members of an overload set share
// a name and are told apart by their parameter types, so the implementation's
// parameter types select the overload; the result type and whether the selected
// overload can be implemented at all are checked afterwards.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}overloads.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import Overloads


// Every member of an overload set can be implemented, whether the members
// differ by parameter type, by arity, or both. An unannotated pointer parameter
// imports as an implicitly unwrapped optional and is implemented as an
// optional, as for any other function.

@cxx @implementation
func overloadedByType(_ x: Int32) -> Int32 { return x }

@cxx @implementation
func overloadedByType(_ x: Double) -> Double { return x }

@cxx @implementation
func overloadedByType(_ p: UnsafeMutablePointer<Int32>?) -> Int32 { return 0 }

@cxx @implementation
func overloadedByArityAndType(_ x: Int32) -> Int32 { return x }

@cxx @implementation
func overloadedByArityAndType(_ x: Double) -> Double { return x }

@cxx @implementation
func overloadedByArityAndType(_ x: Int32, _ y: Int32) -> Int32 { return x + y }


// A `@cxx(...)` name selects among that name's overloads the same way.

@cxx(renamedOverload) @implementation
func swiftRenamedOverloadInt(_ x: Int32) -> Int32 { return x }

@cxx(renamedOverload) @implementation
func swiftRenamedOverloadDouble(_ x: Double) -> Double { return x }


// No overload has the implementation's parameter types.

// expected-error@+1{{could not find imported function 'noMatchingOverload' matching global function 'noMatchingOverload'; make sure you import the module or header that declares it}}
@cxx @implementation
func noMatchingOverload(_ x: Float) -> Float { return x }


// Several overloads import with the same Swift signature, so the
// implementation could be any of them.

// expected-error@+1{{global function 'ambiguousOverload' could implement any of several imported overloads of 'ambiguousOverload' that have the same signature in Swift}}
@cxx @implementation
func ambiguousOverload(_ x: Int32) -> Int32 { return x }


// Duplicate implementations of one overload are diagnosed; an implementation
// of another overload of the same name is not a duplicate.

// expected-note@+2{{previously implemented here}}
@cxx @implementation
func dupOverload(_ x: Int32) -> Int32 { return x }

// expected-error@+1{{duplicate implementation of imported global function 'dupOverload'}}
@cxx(dupOverload) @implementation
func dupOverloadAlias(_ x: Int32) -> Int32 { return x }

@cxx @implementation
func dupOverload(_ x: Double) -> Double { return x }


// The parameter types select the overload; a mismatched result type is then
// diagnosed against that overload.

// expected-error@+2{{global function 'resultMismatchOverload' of type '(Int32) -> Float' does not match type '(CInt) -> CInt' (aka '(Int32) -> Int32') declared by the header}}
@cxx @implementation
func resultMismatchOverload(_ x: Int32) -> Float { return 0 }


// Selection is by parameter types only. Implementing the inline overload is
// rejected as such rather than silently rebound to the other overload.

@cxx @implementation
func partiallyInlineOverload(_ x: Int32) -> Int32 { return x }

// expected-error@+2{{global function 'partiallyInlineOverload' cannot implement C++ function 'partiallyInlineOverload' because it already has a definition}}
@cxx @implementation
func partiallyInlineOverload(_ x: Double) -> Double { return x }
