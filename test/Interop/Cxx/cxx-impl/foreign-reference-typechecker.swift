// C++ foreign reference types as parameter and result types of
// `@cxx @implementation` functions, and as receivers of `@cxx @implementation`
// methods.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}foreign-reference.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import ForeignReference


// A foreign reference type is representable in C++ as a parameter and, when
// returned retained (+1), as a result.

@cxx @implementation
func takesNode(_ n: Node) -> Int32 { return n.value }

@cxx @implementation
func takesNullableNode(_ n: Node?) -> Int32 { return n?.value ?? -1 }

// A C++ reference to a foreign reference type imports like the reference
// type itself. The parameter carries the object, not the reference, so the
// implementation need not be `@unsafe`.
@cxx @implementation
func takesNodeByRef(_ n: Node) -> Int32 { return n.value }

// A C++ reference to a pointer to a foreign reference type is implemented by
// an `inout` parameter of the reference type, a const one by a plain
// parameter. The referent is the pointer, so the implementation must be
// `@unsafe` as for any other reference parameter.
@unsafe @cxx @implementation
func reseatNode(_ p: inout Node, _ to: Node) { p = to }

@unsafe @cxx @implementation
func readNodePtr(_ p: Node) -> Int32 { return p.value }

// expected-error@+2{{global function 'mismatchedNodePtrSpelling' of type '(UnsafeMutablePointer<Node>) -> ()' does not match type '(inout Node) -> Void' declared by the header}}
@unsafe @cxx @implementation
func mismatchedNodePtrSpelling(_ p: UnsafeMutablePointer<Node>) {}

// expected-error@+2{{global function 'missingUnsafeNodePtr' must be marked '@unsafe' to implement C++ function 'missingUnsafeNodePtr'}}{{-1:1-1=@unsafe }}
@cxx @implementation
func missingUnsafeNodePtr(
  _ p: Node // expected-note{{C++ callers may modify parameter 'p' through another reference during the call}}
) -> Int32 { return p.value }

@cxx @implementation
func returnsRetainedNode(_ n: Node) -> Node { return n }

@cxx @implementation
func returnsNullableRetainedNode(_ n: Node, _ null: Int32) -> Node? {
  return null != 0 ? nil : n
}

extension Node {
  @cxx @implementation
  static func passThrough(_ n: Node) -> Node { return n }
}


// A result returned unretained (+0) is not supported yet: the Swift body
// always produces a retained (+1) value.

// expected-error@+2{{global function 'returnsUnretainedNode' cannot implement C++ function 'returnsUnretainedNode' because it returns a foreign reference type unretained ('SWIFT_RETURNS_UNRETAINED'), which is not yet supported}}
@cxx @implementation
func returnsUnretainedNode(_ n: Node) -> Node { return n }

// expected-error@+2{{global function 'returnsUnannotatedNode' cannot implement C++ function 'returnsUnannotatedNode' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
@cxx @implementation
func returnsUnannotatedNode(_ n: Node) -> Node { return n }

// expected-error@+2{{global function 'returnsNodeByRef()' cannot implement C++ function 'returnsNodeByRef' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
@cxx @implementation
func returnsNodeByRef() -> Node { fatalError() }

// expected-error@+2{{global function 'returnsLeaf' cannot implement C++ function 'returnsLeaf' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
@cxx @implementation
func returnsLeaf(_ l: Leaf) -> Leaf { return l }

@cxx @implementation
func returnsRetainedLeaf(_ l: Leaf) -> Leaf { return l }


// An immortal foreign reference type is never retained or released, so its
// result needs no ownership annotation.

@cxx @implementation
func returnsSingleton(_ s: Singleton) -> Singleton { return s }


// Instance methods of a foreign reference type are matched like those of a
// value type, except that a foreign reference type is a class in Swift: a
// non-const method is implemented by a non-mutating method too, which mutates
// the C++ object through the reference.

extension Node {
  @cxx @implementation
  func get() -> Int32 { return value }

  @cxx @implementation
  func add(_ d: Int32) { value += d }

  @cxx @implementation
  func overloadedByType(_ x: Int32) -> Int32 { return value + x }

  @cxx @implementation
  func overloadedByType(_ x: Double) -> Double { return Double(value) + x }

  // A `@cxx(...)` name selects among that name's overloads the same way.
  @cxx(renamedOverload) @implementation
  func swiftRenamedOverloadInt(_ x: Int32) -> Int32 { return value + x }

  @cxx(renamedOverload) @implementation
  func swiftRenamedOverloadDouble(_ x: Double) -> Double { return Double(value) + x }

  // expected-error@+1{{could not find imported function 'notDeclared' matching instance method 'notDeclared()'; make sure you import the module or header that declares it}}
  @cxx @implementation
  func notDeclared() -> Int32 { return value }
}


// A const and a non-const overload with the same parameter types cannot be
// told apart by `mutating`, which a class does not have.

extension Node {
  // expected-error@+1{{instance method 'adjust' could implement any of several imported overloads of 'adjust' that have the same signature in Swift}}
  @cxx @implementation
  func adjust(_ x: Int32) -> Int32 { return value + x }
}


// Methods of an immortal foreign reference type: `self` is never retained or
// released, and a result of the type needs no ownership annotation.

extension Singleton {
  @cxx @implementation
  func read() -> Int32 { return value }

  @cxx @implementation
  func itself() -> Singleton { return self }
}


// A virtual method of a foreign reference type.

extension Polymorphic {
  // expected-error@+2{{instance method 'virtualMethod()' cannot implement C++ method 'virtualMethod' because it is the first non-inline virtual method of C++ class 'Polymorphic'}}
  @cxx @implementation
  func virtualMethod() -> Int32 { return 0 }

  @cxx @implementation
  func nonVirtualMethod() -> Int32 { return 0 }
}


// The retain and release operations of a foreign reference type cannot be
// implemented in Swift: the C++ entry point retains and releases its
// reference-counted parameters and receiver, so the operation would call
// itself.

// expected-error@+2{{@cxx function implementing the retain operation of foreign reference type 'Node' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}
@cxx @implementation
func retainNode(_ n: Node) { }

// expected-error@+2{{@cxx function implementing the release operation of foreign reference type 'Node' will cause infinite recursion; use `Unmanaged` or a pointer type like `UnsafeMutableRawPointer`}}
@cxx @implementation
func releaseNode(_ n: Node) { }

extension Counted {
  // expected-error@+2{{@cxx instance method implementing the retain operation of foreign reference type 'Counted' will cause infinite recursion}}
  @cxx @implementation
  func retainMethod() { }

  // expected-error@+2{{@cxx instance method implementing the release operation of foreign reference type 'Counted' will cause infinite recursion}}
  @cxx @implementation
  func releaseMethod() { }

  @cxx @implementation
  func get() -> Int32 { return value }
}
