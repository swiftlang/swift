// A callee-allocated (yield_once_2) coroutine accessor whose frame cannot be
// stack-allocated is an allocation.  Here the coroutine calling convention is
// disabled, forcing a heap (Malloc) coroutine-frame allocator, so calling such
// an accessor is rejected under -no-allocations.  (When the frame can be stack
// allocated -- the common case -- the call is allowed; see coroutines-and-testing.swift.)

// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature CoroutineAccessors -no-allocations -disable-arm64-corocc -disable-x86_64-corocc -wmo -verify -verify-ignore-unknown

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors

public struct NC: ~Copyable { var x: Int = 0 }

open class Base {
  var _s = NC()
  open var s: NC {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}

func bump(_ x: inout NC) { x.x += 1 }

public func f(_ b: Base) {
  bump(&b.s) // expected-error {{cannot use co-routines (like accessors) in -no-allocations mode}}
}

// A non-virtual accessor is inlined away before IRGen, leaving no coroutine
// frame to allocate, so it is allowed even here (the old pre-inlining check
// rejected it).
public struct S: ~Copyable {
  var _x = NC()
  var x: NC {
    yielding borrow { yield _x }
    yielding mutate { yield &_x }
  }
}

public func g(_ s: inout S) {
  bump(&s.x) // no error
}

// A protocol conformance's coroutine-accessor witness thunk merely forwards its
// caller's allocator (it doesn't allocate a frame itself), so it must not be
// diagnosed -- this is the shape that originally regressed.
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

