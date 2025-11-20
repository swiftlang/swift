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

  // Test borrowing `self`
  public var isEmpty: Bool { i == 0 }

  // Test consuming `self`
  @_lifetime(copy self)
  consuming func derive() -> BV {
    // Technically, this "new" view does not depend on the 'view' argument.
    // This unsafely creates a new view with no dependence.
    let bv = BV(independent: self.p, self.i)
    return unsafeLifetime(dependent: bv, dependsOn: self)
  }
}

@_nonescapable
struct NE {
  var bv: BV

  @_lifetime(copy bv)
  init(_ bv: BV) {
    self.bv = bv
  }
}

struct NC : ~Copyable {
  let p: UnsafeRawPointer
  let i: Int

  func withBV<ResultType>(_ body: (BV) throws -> ResultType) rethrows
    -> ResultType {
    try body(BV(p, i))
  }
}

func takeNoescapeInt(_ f: ()->Int) -> Int { f() }

func bv_extract_pointer(_ bv: BV) -> UnsafeRawPointer {
  return bv.p
}

func bv_argument(bv: BV) -> UnsafeRawPointer {
  return bv_extract_pointer(bv)
}

func bv_capture_noescape(bv: BV) -> Int {
  return takeNoescapeInt { bv.i }
}

func bv_assign_let(_ bv: BV) -> UnsafeRawPointer {
  let local = bv
  return local.p
}

func bv_assign_var(_ bv1: BV, _ bv2: BV, _ z: Bool) -> UnsafeRawPointer {
  var local: BV
  if z {
    local = bv1
  } else {
    local = bv2
  }
  return local.p
}

func bv_assign_field(_ bv: BV) -> UnsafeRawPointer {
  let ne = NE(bv)
  return ne.bv.p
}
