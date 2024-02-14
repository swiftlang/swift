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
  let c: Int

  public var isEmpty: Bool { c == 0 }

  @_unsafeNonescapableResult
  init(_ p: UnsafeRawPointer, _ c: Int) {
    self.p = p
    self.c = c
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let c: Int

  // Requires a borrow.
  borrowing func getBV() -> _borrow(self) BV {
    BV(p, c)
  }
}

// Propagate a borrow.
func bv_get_borrow(container: borrowing NC) -> _borrow(container) BV {
  container.getBV()
}

// Copy a borrow.
func bv_get_copy(container: borrowing NC) -> _copy(container) BV {
  return container.getBV()
}

// Recognize nested accesses as part of the same dependence scope.
func bv_get_mutate(container: inout NC) -> _mutate(container) BV {
  container.getBV()
}
