// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics

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

  consuming func derive() -> _consume(self) BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    return BV(self.p, self.i)
  }
}

@_nonescapable
struct NE {
  var bv: BV

  init(_ bv: consuming BV) -> _consume(bv) Self {
    self.bv = bv
    return self
  }
}

func bv_derive_local(bv: consuming BV) -> _consume(bv) BV {
  let bv2 = BV(bv.p, bv.i)
  return bv2.derive() // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-2 {{it depends on the lifetime of variable 'bv2'}}
  // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
}
