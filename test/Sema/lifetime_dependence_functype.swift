// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonescapableTypes
// REQUIRES: asserts

struct NC: ~Copyable {
  var ne: NE {
    NE()
  }
}

struct NE: ~Escapable {
  @_unsafeNonescapableResult
  init() {}
}

func transfer(_ ne: NE) -> NE {
  ne
}

func applyAnnotatedTransfer(ne: NE, @lifetime(0) transfer: (NE) -> NE) -> NE { // expected-error{{'@lifetime' attribute cannot be applied to this declaration}}
  transfer(ne)
}

func applyTransfer(ne: NE, transfer: (NE) ->  NE) -> NE {
  transfer(ne)
}

func testTransfer(nc: consuming NC) {
  let transferred = applyTransfer(ne: nc.ne, transfer: transfer) // expected-error{{cannot convert value of type '(NE) -> @lifetime(copy 0) NE' to expected argument type '(NE) -> NE'}}

  _ = consume nc
  _ = transfer(transferred)
}

func borrow(_ nc: borrowing NC) -> NE {
  nc.ne
}

func applyBorrow(nc: borrowing NC, borrow: (borrowing NC) -> NE) -> NE {
  borrow(nc)
}

func testBorrow(nc: consuming NC) {
  let borrowed = applyBorrow(nc: nc, borrow: borrow) // expected-error{{cannot convert value of type '(borrowing NC) -> @lifetime(borrow 0) NE' to expected argument type '(borrowing NC) -> NE}}
  _ = consume nc
  _ = transfer(borrowed)
}

