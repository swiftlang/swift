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

  @_lifetime(copy self)
  consuming func derive() -> BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    let bv = BV(independent: self.p, self.i)
    return unsafeLifetime(dependent: bv, dependsOn: self)
  }
}

// Nonescapable wrapper.
struct NEBV : ~Escapable {
  var bv: BV

  // Test lifetime inheritance through initialization.
  @_lifetime(copy bv)
  init(_ bv: consuming BV) {
    self.bv = bv
  }

  var view: BV {
    @_lifetime(copy self)
    _read {
      yield bv
    }
    @_lifetime(&self)
    _modify {
      yield &bv
    }
  }

  @_lifetime(borrow self)
  func borrowedView() -> BV {
    bv
  }
}

// Test lifetime inheritance through chained consumes.
@_lifetime(copy bv)
func bv_derive(bv: consuming BV) -> BV {
  bv.derive()
}

// Test lifetime inheritance through stored properties.
@_lifetime(copy nebv)
func ne_extract_member(nebv: consuming NEBV) -> BV {
  return nebv.bv
}

@_lifetime(copy nebv)
func ne_yield_member(nebv: consuming NEBV) -> BV {
  return nebv.view
}

func bv_consume(_ x: consuming BV) {}

// It is ok to consume the aggregate before the property.
// The property's lifetime must exceed the aggregate's.
func nebv_consume_member(nebv: consuming NEBV) {
  let bv = nebv.bv
  _ = consume nebv
  bv_consume(bv)
}

func nebv_consume_after_yield(nebv: consuming NEBV) {
  let view = nebv.view
  _ = consume nebv
  bv_consume(view)
}
