// A yield_once_1 (_read/_modify) coroutine accessor uses a heap-allocated frame,
// so calling one is rejected under -no-allocations.  (A callee-allocated
// yield_once_2 accessor is allowed; see no-allocations-coroutine.swift.)

// RUN: not %target-swift-emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature CoroutineAccessors -no-allocations -wmo 2>&1 | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_CoroutineAccessors

// CHECK: cannot use co-routines (like accessors) in -no-allocations mode

open class Base {
  var _i = 42
  // Dispatched through the class vtable, so the accessor survives inlining.
  open var i: Int {
    _read { yield _i }
    _modify { yield &_i }
  }
}

func bump(_ x: inout Int) { x += 1 }

public func f(_ b: Base) { bump(&b.i) }
