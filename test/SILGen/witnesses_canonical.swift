// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// rdar://problem/20714534 -- we need to canonicalize an associated type's
// protocols when emitting witness method table for a conformance.

public protocol _ST {}

public protocol ST : _ST {}

public protocol _CDT : ST {}

public struct _S<T : _CDT> : CT {
  internal init(a: T, b: Int) {}
}

extension _CDT {
  final public subscript(b: Int) -> _S<Self> {
    return _S(a: self, b: b)
  }
}

public protocol CT : ST, _CDT {
  associatedtype _SS : ST, _CDT

  subscript(_bounds: Int) -> _SS { get }
}

// CHECK: sil_witness_table <T where T : _CDT> _S<T>: CT module witnesses_canonical {
// associated_type_protocol (_SS: _CDT): _S<_S<T>>: specialize <T = _S<T>> (<T where T : _CDT> _S<T>: _CDT module witnesses_canonical)

// rdar://problem/21599502 -- make sure we get requirements on
// associated types from protocols we inherit.
public protocol P1 { }
public protocol P2 { }

public protocol Q1 {
  associatedtype Assoc : P1
}

public protocol Q2 {
  associatedtype Assoc : P2
}

public protocol Q3 : Q1, Q2 {
  associatedtype Assoc
}

struct XP : P1, P2 { }

struct XQ3 : Q3 {
  typealias Assoc = XP
}

// CHECK: sil_witness_table XQ3: Q3 module witnesses_canonical {
// CHECK:  associated_type Assoc: XP
// CHECK:  associated_type_protocol (Assoc: P1): XP: P1 module witnesses_canonical
// CHECK:  associated_type_protocol (Assoc: P2): XP: P2 module witnesses_canonical
// CHECK: }
