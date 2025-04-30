// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let c: Int

  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let c: Int

  func withBV<ResultType>(_ body: (BV) throws -> ResultType) rethrows
    -> ResultType {
    try body(BV(p, c))
  }
}

@_nonescapable
struct NE {
  var bv: BV

  @lifetime(copy bv)
  init(_ bv: consuming BV) {
    self.bv = bv
  }
}

@lifetime(other: copy bv)
func bv_assign_inout_copy(bv: BV, other: inout BV) {
  other = bv // OK
}

@lifetime(other: borrow bv)
func bv_assign_inout_borrow(bv: BV, other: inout BV) {
  other = bv
}

@lifetime(bv: copy bv)
@lifetime(other: copy bv)
func bvmut_assign_inout(bv: inout BV, other: inout BV) {
  other = bv
}

@lifetime(other: copy bv)
func bvcons_assign_inout(bv: consuming BV, other: inout BV) {
  other = bv
}

@lifetime(other: copy bv)
func bv_assign_field(bv: BV, other: inout NE) {
  other.bv = bv
}

@lifetime(bv: copy bv)
@lifetime(other: copy bv)
func bvmut_assign_field(bv: inout BV, other: inout NE) {
  other.bv = bv
}

@lifetime(other: copy bv)
func bvcons_assign_field(bv: consuming BV, other: inout NE) {
  other.bv = bv
}

func bv_capture_escape(bv: BV) -> ()->Int { // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of argument 'bv'}}
  return { bv.c } // expected-note {{this use causes the lifetime-dependent value to escape}}
}

// FIXME: Our debug locations on closure captures are incorrect.
// FIXME: We return two errors: one for the caller, and one for the closure.
func bvcons_capture_escape(bv: consuming BV) -> ()->Int { // expected-error *{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 *{{it depends on the lifetime of argument 'bv'}}
  // expected-note @-2 *{{this use causes the lifetime-dependent value to escape}}
  return { bv.c }
}

func bv_capture_escapelet(bv: BV) -> ()->Int { // expected-error{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 {{it depends on the lifetime of argument 'bv'}}
  let closure = { bv.c } // expected-note {{this use causes the lifetime-dependent value to escape}}
  return closure
}

// FIXME: Our debug locations on closure captures are incorrect.
// FIXME: We return two errors: one for the caller, and one for the closure.
func bvcons_capture_escapelet(bv: consuming BV) -> ()->Int { // expected-error *{{lifetime-dependent variable 'bv' escapes its scope}}
  // expected-note @-1 *{{it depends on the lifetime of argument 'bv'}}
  // expected-note @-2 *{{this use causes the lifetime-dependent value to escape}}
  let closure = { bv.c }
  return closure
}
