// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int
  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
  @_lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }

  // @lifetime(borrow self)
  borrowing func getEmpty() -> Empty {
    Empty()
  }
}

// Test dependencies on an empty struct.
public struct Empty: ~Escapable {
  @_lifetime(immortal)
  init() {}
}

func use(e: Empty) {}

struct NE : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
  @_lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }
}

@_lifetime(copy container)
func bv_get_consume(container: consuming NE) -> BV {
  return container.getBV() // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1{{it depends on this scoped access to variable 'container'}}
    // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
}

struct Wrapper : ~Escapable {
  let bv: BV
}

@_lifetime(copy bv2)
func bv_incorrect_annotation1(_ bv1: borrowing BV, _ bv2: borrowing BV) -> BV { // expected-error {{lifetime-dependent variable 'bv1' escapes its scope}}
  return copy bv1                                                                              // expected-note @-1{{it depends on the lifetime of argument 'bv1'}}
}                                                                                              // expected-note @-1{{this use causes the lifetime-dependent value to escape}}

@_lifetime(copy w2)
func bv_incorrect_annotation2(_ w1: borrowing Wrapper, _ w2: borrowing Wrapper) -> BV { // expected-error {{lifetime-dependent variable 'w1' escapes its scope}}
  return w1.bv                                                                                        // expected-note @-1{{it depends on the lifetime of argument 'w1'}}
}                                                                                                     // expected-note @-1{{this use causes the lifetime-dependent value to escape}}

func testEmpty(nc: consuming NC) {
  var e: Empty // expected-error {{lifetime-dependent variable 'e' escapes its scope}}
  do {
    let inner = nc // expected-note {{it depends on the lifetime of variable 'inner'}}
    e = inner.getEmpty()
  }
  use(e: e) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}
