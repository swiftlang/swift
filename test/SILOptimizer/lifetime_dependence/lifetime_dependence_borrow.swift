// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_LifetimeDependence

@_unsafeNonescapableResult
@lifetime(copy source)
internal func _overrideLifetime<
  T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable
>(
  _ dependent: consuming T, copying source: borrowing U
) -> T {
  dependent
}

// Some container-ish thing.
struct CN: ~Copyable {
  let p: UnsafeRawPointer
  let i: Int
}

// Some Bufferview-ish thing.
struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  public var isEmpty: Bool { i == 0 }

  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }

  init(_ cn: borrowing CN) {
    self.p = cn.p
    self.i = cn.i
  }
}

// Some MutableBufferview-ish thing.
struct MBV : ~Escapable, ~Copyable {
  let p: UnsafeRawPointer
  let i: Int
  
  @lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }

  // Requires a borrow.
  @lifetime(copy self)
  borrowing func getBV() -> BV {
    let bv = BV(p, i)
    return _overrideLifetime(bv, copying: self)
  }
}

// Nonescapable wrapper.
struct NEBV : ~Escapable {
  var bv: BV

  @lifetime(copy bv)
  init(_ bv: consuming BV) {
    self.bv = bv
  }
}

// Propagate a borrow.
@lifetime(copy container)
func bv_get_borrow(container: borrowing MBV) -> BV {
  container.getBV()
}

// Copy a borrow.
@lifetime(copy container)
func bv_get_copy(container: borrowing MBV) -> BV {
  return container.getBV()
}

// Recognize nested accesses as part of the same dependence scope.
@lifetime(copy container)
func bv_get_mutate(container: inout MBV) -> BV {
  container.getBV()
}

// Create and decompose a nonescapable aggregate.
@lifetime(borrow cn)
func ne_wrap_and_extract_member(cn: borrowing CN) -> BV {
  let bv = BV(cn)
  let ne = NEBV(bv)
  return ne.bv
}
