// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes

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

  consuming func derive() -> dependsOn(self) BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    return BV(self.p, self.i)
  }
}

@_nonescapable
struct NE {
  var bv: BV

  init(_ bv: consuming BV) -> dependsOn(bv) Self {
    self.bv = bv
    return self
  }
}
