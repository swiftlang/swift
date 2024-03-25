// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

struct BV : ~Escapable {
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

struct NE : ~Escapable {
  var bv: BV

  // Test lifetime inheritance through initialization.
  init(_ bv: consuming BV) -> _consume(bv) Self {
    self.bv = bv
    return self
  }
}

// Test lifetime inheritance through chained consumes.
func bv_derive(bv: consuming BV) -> _consume(bv) BV {
  bv.derive()
}

// Test lifetime inheritance through stored properties.
func ne_extract_member(ne: consuming NE) -> _consume(ne) BV {
  return ne.bv
}
