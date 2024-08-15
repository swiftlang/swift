// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

// TODO: Remove @_unsafeNonescapableResult. Instead, the unsafe dependence should be expressed by a builtin that is
// hidden within the function body.
@_unsafeNonescapableResult
func unsafeLifetime<T: ~Copyable & ~Escapable, U: ~Copyable & ~Escapable>(
  dependent: consuming T, dependsOn source: borrowing U)
  -> dependsOn(source) T {
  dependent
}

struct BV : ~Escapable {
  let p: UnsafeRawPointer
  let i: Int

  init(_ p: UnsafeRawPointer, _ i: Int) -> dependsOn(p) Self {
    self.p = p
    self.i = i
  }

  init(independent p: UnsafeRawPointer, _ i: Int) -> dependsOn(p) Self {
    self.p = p
    self.i = i
  }

  consuming func derive() -> dependsOn(self) BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    let bv = BV(independent: self.p, self.i)
    return unsafeLifetime(dependent: bv, dependsOn: self)
  }
}

struct NE : ~Escapable {
  var bv: BV

  init(_ bv: consuming BV) -> dependsOn(bv) Self {
    self.bv = bv
    return self
  }
}
