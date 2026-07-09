// A callee-allocated (yield_once_2) coroutine accessor allocates its frame on
// the caller's stack, so using one is allowed under -no-allocations -- whether
// it is inlined, dispatched through a class vtable, or reached via a protocol
// conformance's (forwarding) witness thunk.  (A yield_once_1 accessor, which
// uses a heap-allocated frame, is still rejected; see
// no-allocations-coroutine-legacy.swift.)

// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature CoroutineAccessors -no-allocations -wmo

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors

public struct NC: ~Copyable { var x = 0 }

func bump(_ x: inout NC) { x.x += 1 }

// Non-virtual: the accessor is inlined away.
public struct S: ~Copyable {
  var _x = NC()
  var x: NC {
    yielding borrow { yield _x }
    yielding mutate { yield &_x }
  }
}
public func viaStruct(_ s: inout S) { bump(&s.x) }

// Virtual (class vtable): the accessor survives inlining.
open class Base {
  var _s = NC()
  open var s: NC {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}
public func viaClass(_ b: Base) { bump(&b.s) }

// Protocol conformance: the coroutine-accessor witness thunk forwards its
// caller's allocator.
public protocol Q {
  var v: Int { yielding borrow set }
}
public struct Conformer: Q {
  var _v = 0
  public var v: Int {
    yielding borrow { yield _v }
    yielding mutate { yield &_v }
  }
}
