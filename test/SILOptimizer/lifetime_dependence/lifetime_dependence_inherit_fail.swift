// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature Lifetimes

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

// TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence should be expressed by a builtin that is
// hidden within the function body.
@_unsafeNonescapableResult
@_lifetime(copy source)
func unsafeLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  dependent: consuming T, dependsOn source: borrowing U)
  -> T {
  dependent
}

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  @_lifetime(borrow p)
  init(_ p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }

  @_lifetime(borrow p)
  init(independent p: UnsafeRawPointer, _ i: Int) {
    self.p = p
    self.i = i
  }

  @_lifetime(borrow self)
  consuming func derive() -> BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    let bv = BV(independent: self.p, self.i)
    return unsafeLifetime(dependent: bv, dependsOn: self)
  }
}

struct NE : ~Escapable {
  var bv: BV

  @_lifetime(copy bv)
  init(_ bv: consuming BV) {
    self.bv = bv
  }
}
