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
// type itself.
@cxx @implementation
func takesNodeByRef(_ n: Node) -> Int32 { return n.value }

// A C++ reference to a pointer to a foreign reference type is a pointer to
// that pointer.
@cxx @implementation
func reseatNode(_ p: UnsafeMutablePointer<Node>, _ to: Node) { p.pointee = to }

@cxx @implementation
func readNodePtr(_ p: UnsafePointer<Node>) -> Int32 { return p.pointee.value }

// expected-error@+2{{global function 'mismatchedNodePtrSpelling' of type '(Node) -> ()' does not match type '(UnsafeMutablePointer<Node>) -> Void' declared by the header}}
@cxx @implementation
func mismatchedNodePtrSpelling(_ p: Node) {}

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


// A virtual method of a foreign reference type.

extension Polymorphic {
  // expected-error@+2{{instance method 'virtualMethod()' cannot implement C++ function 'virtualMethod' because it is its class's key function, and Swift cannot yet emit the class's vtable, which C++ emits in the translation unit that defines the key function; declare another out-of-line virtual method earlier in the class to make that method the key function}}
  @cxx @implementation
  func virtualMethod() -> Int32 { return 0 }

  @cxx @implementation
  func nonVirtualMethod() -> Int32 { return 0 }
}
