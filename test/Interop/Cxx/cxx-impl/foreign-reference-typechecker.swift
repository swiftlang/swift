// C++ foreign reference types as parameter and result types of
// `@cxx @implementation` functions.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import ForeignReference


// A foreign reference type is representable in C++ as a parameter and, when
// returned retained (+1), as a result.

@cxx @implementation
func takesNode(_ n: Node) -> Int32 { return n.value }

@cxx @implementation
func takesNullableNode(_ n: Node?) -> Int32 { return n?.value ?? -1 }

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

// expected-error@+2{{global function 'returnsLeaf' cannot implement C++ function 'returnsLeaf' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
@cxx @implementation
func returnsLeaf(_ l: Leaf) -> Leaf { return l }

@cxx @implementation
func returnsRetainedLeaf(_ l: Leaf) -> Leaf { return l }


// An immortal foreign reference type is never retained or released, so its
// result needs no ownership annotation.

@cxx @implementation
func returnsSingleton(_ s: Singleton) -> Singleton { return s }
