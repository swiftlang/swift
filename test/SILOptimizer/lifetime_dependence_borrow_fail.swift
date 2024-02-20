// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics \
// RUN:   -enable-experimental-lifetime-dependence-inference

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

@_nonescapable
struct BV {
  let p: UnsafeRawPointer
  let i: Int

  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
  borrowing func getBV() -> _borrow(self) BV {
    BV(p, i)
  }
}

@_nonescapable
struct NE {
  let p: UnsafeRawPointer
  let i: Int

  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }
  borrowing func getBV() -> _borrow(self) BV {
    BV(p, i)
  }
}

func bv_get_consume(container: consuming NE) -> BV {
  return container.getBV() // expected-error {{lifetime-dependent value escapes its scope}}
    // expected-note @-1{{it depends on this scoped access to variable 'container'}}
    // expected-note @-2{{this use causes the lifetime-dependent value to escape}}
}

