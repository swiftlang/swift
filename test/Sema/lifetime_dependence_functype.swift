// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

struct NC: ~Copyable {
  var ne: NE {
    NE()
  }
}

struct NE: ~Escapable {
  @_unsafeNonescapableResult
  init() {}
}

@_lifetime(copy ne)
func transfer(_ ne: NE) -> NE {
  ne
}

@_lifetime(copy ne)
func applyAnnotatedTransfer(ne: NE, @_lifetime(0) transfer: (NE) -> NE) -> NE { // expected-error{{'@_lifetime' attribute cannot be applied to this declaration}}
  transfer(ne)
}

@_lifetime(copy ne)
func applyTransfer(ne: NE, transfer: (NE) ->  NE) -> NE {
  transfer(ne)
}

func testTransfer(nc: consuming NC) {
  let transferred = applyTransfer(ne: nc.ne, transfer: transfer) // expected-error{{does not conform to expected type 'Escapable'}} e/xpected-error{{cannot convert value of type '(NE) -> @_lifetime(copy 0) NE' to expected argument type '(NE) -> NE'}}

  _ = consume nc
  _ = transfer(transferred)
}

func borrow(_ nc: borrowing NC) -> NE {
  nc.ne
}

@_lifetime(borrow nc)
func applyBorrow(nc: borrowing NC, borrow: (borrowing NC) -> NE) -> NE {
  borrow(nc)
}

func testBorrow(nc: consuming NC) {
  let borrowed = applyBorrow(nc: nc, borrow: borrow) // expected-error{{does not conform to expected type 'Escapable'}} ex/pected-error{{cannot convert value of type '(borrowing NC) -> @_lifetime(borrow 0) NE' to expected argument type '(borrowing NC) -> NE}}
  _ = consume nc
  _ = transfer(borrowed)
}

