// RUN: %target-typecheck-verify-swift

enum MyError: Error {
  case fail
}

enum MyBadError {
  case fail
}

func testClosures() {
  let c1 = { () throws(MyError) in
    throw .fail
  }

  let _: () -> Void = c1
  // expected-error@-1{{invalid conversion from throwing function of type '() throws(MyError) -> ()'}}

  let _: () throws(MyError) -> Void = {
    // NOTE: under full typed throws, this should succeed
    throw MyError.fail // expected-error{{thrown expression type 'any Error' cannot be converted to error type 'MyError'}}
  }

  let _: () throws(MyError) -> Void = {
    throw .fail // expected-error{{type 'any Error' has no member 'fail'}}
  }

  // FIXME: Terrible diagnostic.
  // expected-error@+1{{unable to infer closure type without a type annotation}}
  let _ = { () throws(MyBadError) in
    throw MyBadError.fail
  }

  // We do not infer thrown error types from the body, because doing so would
  // break existing code.
  let c2 = { throw MyError.fail }
  let _: Int = c2
  // expected-error@-1{{cannot convert value of type '() throws -> ()'}}
}

