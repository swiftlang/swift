// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   -enable-experimental-feature AddressableParameters

// RUN: %target-swift-frontend %s -emit-silgen \
// RUN:   -sil-verify-all \
// RUN:   -enable-builtin-module \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature AddressableTypes \
// RUN:   -enable-experimental-feature AddressableParameters \
// RUN:   2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_AddressableTypes
// REQUIRES: swift_feature_AddressableParameters

import Builtin

class C {
  var p: Builtin.RawPointer
  init(p: Builtin.RawPointer) { self.p = p }

  // unsupported annotation
  @_addressableSelf
  func method() {}
}

struct NE: ~Escapable {
  var p: Builtin.RawPointer
}

struct PtrHolder {
  var field: Builtin.RawPointer
}

struct CHolder {
  var field: C
}

struct IHolder {
  var field: Int
}

@_lifetime(borrow ptr)
func depends(onPtr ptr: Builtin.RawPointer) -> NE {
  NE(p: ptr)
}

// FIXME: Allowed now, but should be prohibited if projection dependence carries exclusivity.
// rdar://153670930 ([nonescapable] extend access scopes for trivial value (UnsafePointer) dependencies)
@_lifetime(&holder)
func testTrivialField(holder: inout PtrHolder, other: Builtin.RawPointer) -> NE {
  // copy holder.field
  // end_access holder.field
  let ne = depends(onPtr: holder.field)
  // mark_dependence ne on holder
  holder.field = other
  return ne
}

@_lifetime(borrow c)
func depends(onC c: C) -> NE {
  let ne = NE(p: c.p)
  return _overrideLifetime(ne, borrowing: c)
}

// OK: Correctly diagnoses the exclusivity violation.
@_lifetime(&holder)
func testObjectField(holder: inout CHolder, other: C) -> NE {
  // copy holder.field
  // end_access holder.field
  let ne = depends(onC: holder.field) // expected-note{{conflicting access is here}}
  // mark_dependence ne on holder
  holder.field = other // expected-error{{overlapping accesses to 'holder.field', but modification requires exclusive access; consider copying to a local variable}}
  return ne
}

// OK
@_lifetime(&holder)
func testAddress(holder: inout IHolder) -> NE {
  // Requires rdar://137608270 ([borrows] Add Builtin.addressof() support for @addressable arguments)
  depends(onPtr: Builtin.addressOfBorrow(holder))
}

@_lifetime(borrow i)
func depends(onInt i: @_addressable Int) -> NE {
  NE(p: Builtin.addressOfBorrow(i))
}

// OK: no escape diagnostic because the addressable dependency is on the parameter address.
@_lifetime(&holder)
func testAddressableTrivialField(holder: inout IHolder) -> NE{
  // load holder.field
  // end_access holder.field
  // alloc_stack Int
  // store
  return depends(onInt: holder.field)
  // mark_dependence on alloc_stack
  // (the addressable dependence needs to be on the passed-in address)
}

@_lifetime(borrow x)
func depends(onCAddress x: @_addressable C) -> NE {
  let ne = NE(p: x.p)
  return _overrideLifetime(ne, borrowing: x)
}

// OK: no escape diagnostic because the addressable dependency is on the parameter address.
@_lifetime(&holder)
func testAddressableObjectField(holder: inout CHolder) -> NE {
  // tmp = load [copy] holder.field
  // end_access holder.field
  // alloc_stack
  // store tmp
  return depends(onCAddress: holder.field)
  // mark_dependence ne on alloc_stack
}

// Copyable to test the absence of temporaries.
@_addressableForDependencies
struct Cell<T> {
  var t: T
}

struct CellHolder: ~Copyable {
  var field: Cell<Int>
}

@_lifetime(borrow x)
func depends(onCell x: borrowing Cell<Int>) -> NE {
  NE(p: Builtin.addressOfBorrow(x))
}

// OK: no escape diagnostic because the addressable dependency is on the parameter address.
@_lifetime(&holder)
func testAddressableType(holder: inout CellHolder) -> NE {
  // tmp = load [copy] holder.field
  // end_access holder.field
  // alloc_stack
  // store tmp
  return depends(onCell: holder.field)
  // mark_dependence ne on alloc_stack
}

/* FIXME: Invalid SIL: rdar://152273896 (SILGen: @_addressableSelf support for class methods)
func testAddressableClass(c: C) {
  c.method()
}
*/

struct Object {
  var c: C

  @_addressableSelf
  func method() {}
}

struct UniquePointer<T>: ~Copyable {
  var _p: UnsafePointer<T>

  var value: T { unsafeAddress { _p } }
}

// No copy allowed. A copy would be invalid when class 'C' is imported from C++.
//
// CHECK-LABEL: sil hidden [ossa] @$s4test0A15AddressableSelf2upyAA13UniquePointerVyAA6ObjectVG_tF : $@convention(thin) (@guaranteed UniquePointer<Object>) -> () {
// CHECK: bb0(%0 : @guaranteed $UniquePointer<Object>):
// CHECK:   [[CP:%[0-9]+]] = copy_value %0
// CHECK:   [[MNC:%[0-9]+]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[CP]]
// CHECK:   [[BB:%[0-9]+]] = begin_borrow [[MNC]]
// CHECK:   [[PTR:%[0-9]+]] = struct_extract %{{.*}}, #UnsafePointer._rawValue
// CHECK:   [[ADR:%[0-9]+]] = pointer_to_address [[PTR]] to [strict] $*Object
// CHECK:   [[MD:%[0-9]+]] = mark_dependence [unresolved] [[ADR]] on [[BB]]
// CHECK-NOT: load
// CHECK-NOT: alloc_stack
// CHECK-NOT: store
// CHECK:   apply %{{.*}}([[MD]]) : $@convention(method) (@in_guaranteed Object) -> ()
// CHECK-LABEL: } // end sil function '$s4test0A15AddressableSelf2upyAA13UniquePointerVyAA6ObjectVG_tF'
func testAddressableSelf(up: borrowing UniquePointer<Object>) {
  // tmp = load [copy] up.value
  // end_access up.value
  // alloc_stack
  // store tmp
  // apply method(tmp)
  up.value.method()
}
