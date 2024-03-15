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
  let c: Int

  public var isEmpty: Bool { c == 0 }

  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }
}

@_nonescapable
struct NCNE : ~Copyable {
  let p: UnsafeRawPointer
  let c: Int
  
  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }

  // Requires a borrow.
  borrowing func getBV() -> _borrow(self) BV {
    BV(p, c)
  }
}

// Propagate a borrow.
func bv_get_borrow(container: borrowing NCNE) -> _borrow(container) BV {
  container.getBV()
}

// Copy a borrow.
func bv_get_copy(container: borrowing NCNE) -> _copy(container) BV {
  return container.getBV()
}

// Recognize nested accesses as part of the same dependence scope.
func bv_get_mutate(container: inout NCNE) -> _mutate(container) BV {
  container.getBV()
}
