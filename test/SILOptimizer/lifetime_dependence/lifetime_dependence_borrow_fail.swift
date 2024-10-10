// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -disable-experimental-parser-round-trip
// FIXME: Remove '-disable-experimental-parser-round-trip' (rdar://137636751).

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int
  @lifetime(borrow p)
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
  @lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }

  borrowing func getEmpty() -> Empty {
    Empty()
  }
}

// Test dependencies on an empty struct.
public struct Empty: ~Escapable {}

func use(e: Empty) {}

struct NE : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
  @lifetime(borrow self)
  borrowing func getBV() -> BV {
    BV(p, i)
  }
}

func bv_get_consume(container: consuming NE) -> BV {
  return container.getBV() // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1{{it depends on this scoped access to variable 'container'}}
    // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
}

struct Wrapper : ~Escapable {
  let bv: BV
}

@lifetime(bv2)
func bv_incorrect_annotation1(_ bv1: borrowing BV, _ bv2: borrowing BV) -> BV { // expected-error {{lifetime-dependent variable 'bv1' escapes its scope}}
  return copy bv1                                                                              // expected-note @-1{{it depends on the lifetime of argument 'bv1'}}
}                                                                                              // expected-note @-1{{this use causes the lifetime-dependent value to escape}}

@lifetime(w2)
func bv_incorrect_annotation2(_ w1: borrowing Wrapper, _ w2: borrowing Wrapper) -> BV { // expected-error {{lifetime-dependent variable 'w1' escapes its scope}}
  return w1.bv                                                                                        // expected-note @-1{{it depends on the lifetime of argument 'w1'}}
}                                                                                                     // expected-note @-1{{this use causes the lifetime-dependent value to escape}}

let ptr = UnsafeRawPointer(bitPattern: 1)!
let nc = NC(ptr, 0) // expected-error {{lifetime-dependent variable 'nc' escapes its scope}}

func bv_global(dummy: BV) -> BV {
  nc.getBV()
} // expected-note {{this use causes the lifetime-dependent value to escape}}

func testEmpty(nc: consuming NC) {
  var e: Empty // expected-error {{lifetime-dependent variable 'e' escapes its scope}}
  do {
    let inner = nc // expected-note {{it depends on the lifetime of variable 'inner'}}
    e = inner.getEmpty()
  }
  use(e: e) // expected-note {{this use of the lifetime-dependent value is out of scope}}
}
