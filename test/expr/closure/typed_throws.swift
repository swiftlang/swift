// RUN: %target-typecheck-verify-swift

enum MyError: Error {
  case fail
}

enum MyBadError {
  case fail
}

struct GenericError<T>: Error {}

func testThrowsOverloadedOnNever() {
  func foo(_ fn: () throws -> Void) {}
  func foo(_ fn: () throws(Never) -> Void) {}

  func bar() throws {}

  foo {
    try bar() // Ok
  }
}

func testClosures() {
  let c1 = { () throws(MyError) in
    throw .fail
  }

  let _: () -> Void = c1
  // expected-error@-1{{invalid conversion from throwing function of type '() throws(MyError) -> ()'}}

  let _: () throws(MyError) -> Void = {
    throw MyError.fail // Ok
  }

  let _: () throws(MyError) -> Void = {
    throw .fail // Ok
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

  _ = { () throws(_) in } // expected-error {{type placeholder not allowed here}}
  _ = { () throws(GenericError<_>) in } // expected-error {{type placeholder not allowed here}}
}

