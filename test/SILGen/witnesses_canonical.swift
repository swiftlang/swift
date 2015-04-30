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
  typealias _SS : ST, _CDT

  subscript(_bounds: Int) -> _SS { get }
}

// CHECK: sil_witness_table <T where T : _CDT> _S<T>: CT module witnesses_canonical {
// associated_type_protocol (_SS: _CDT): _S<_S<T>>: specialize <T = _S<T>> (<T where T : _CDT> _S<T>: _CDT module witnesses_canonical)
