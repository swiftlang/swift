// A protocol requirement may now be spelled with `yielding mutate` (previously
// only `yielding borrow` was allowed on the read side, with the write side
// restricted to `set` or the `mutate` accessor of the `borrow`/`mutate`
// family).  This exercises `yielding mutate` requirements: witness-table
// layout, and dispatch through both generic and existential access.

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors

public protocol P {
  var x: Int { yielding borrow yielding mutate }
}

public struct S: P {
  var _s = 0
  public var x: Int {
    yielding borrow { yield _s }
    yielding mutate { yield &_s }
  }
}

@_silgen_name("readGeneric")
public func readGeneric<T: P>(_ t: T) -> Int {
// CHECK-LABEL: sil {{.*}}@readGeneric : {{.*}} {
// CHECK:         witness_method {{.*}}#P.x!yielding_borrow
// CHECK-LABEL: } // end sil function 'readGeneric'
  return t.x
}

@_silgen_name("modifyGeneric")
public func modifyGeneric<T: P>(_ t: inout T) {
// CHECK-LABEL: sil {{.*}}@modifyGeneric : {{.*}} {
// CHECK:         witness_method {{.*}}#P.x!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyGeneric'
  t.x += 1
}

@_silgen_name("readExistential")
public func readExistential(_ t: any P) -> Int {
// CHECK-LABEL: sil {{.*}}@readExistential : {{.*}} {
// CHECK:         witness_method {{.*}}#P.x!yielding_borrow
// CHECK-LABEL: } // end sil function 'readExistential'
  return t.x
}

@_silgen_name("modifyExistential")
public func modifyExistential(_ t: inout any P) {
// CHECK-LABEL: sil {{.*}}@modifyExistential : {{.*}} {
// CHECK:         witness_method {{.*}}#P.x!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyExistential'
  t.x += 1
}

// The write side may also pair with a plain getter, mirroring how the read
// side may pair with a plain setter (`yielding borrow set`).
public protocol Q {
  var y: Int { get yielding mutate }
}

public struct T: Q {
  var _t = 0
  public var y: Int {
    get { _t }
    yielding mutate { yield &_t }
  }
}

// An explicitly written `set` is honored alongside the coroutine: the
// requirement keeps its plain setter slot, so adding `yielding mutate` to an
// existing `{ get set }` requirement stays additive rather than removing the
// setter from the witness layout.
public protocol R {
  var z: Int { get set yielding mutate }
}

// A conformer need not write the coroutine; it is synthesized from `set`.
public struct U: R {
  public var z: Int = 0
}

@_silgen_name("assignR")
public func assignR<V: R>(_ v: inout V) {
// CHECK-LABEL: sil {{.*}}@assignR : {{.*}} {
// CHECK:         witness_method {{.*}}#R.z!setter
// CHECK-LABEL: } // end sil function 'assignR'
  v.z = 1
}

@_silgen_name("modifyR")
public func modifyR<V: R>(_ v: inout V) {
// CHECK-LABEL: sil {{.*}}@modifyR : {{.*}} {
// CHECK:         witness_method {{.*}}#R.z!yielding_mutate
// CHECK-LABEL: } // end sil function 'modifyR'
  v.z += 1
}

// Witness tables are emitted after all function bodies.  A pure-coroutine
// requirement has no plain getter/setter of its own -- S's witness table lists
// only the coroutine slots, symmetric with a `{ yielding borrow }`-only (or
// `@_borrowed`) read-only requirement; T's pairs a plain getter with the
// coroutine write; U's keeps the written setter alongside it.
// CHECK-LABEL: sil_witness_table [serialized] S: P module m {
// CHECK-NEXT:    method #P.x!yielding_borrow
// CHECK-NEXT:    method #P.x!yielding_mutate
// CHECK-NEXT:  }
// CHECK-LABEL: sil_witness_table [serialized] T: Q module m {
// CHECK-NEXT:    method #Q.y!getter
// CHECK-NEXT:    method #Q.y!yielding_mutate
// CHECK-NEXT:  }
// CHECK-LABEL: sil_witness_table [serialized] U: R module m {
// CHECK-NEXT:    method #R.z!getter
// CHECK-NEXT:    method #R.z!setter
// CHECK-NEXT:    method #R.z!yielding_mutate
// CHECK-NEXT:  }
