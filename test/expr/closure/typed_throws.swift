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

  let _: () throws(Never) -> Void = { () throws in } // Ok
  // expected-error@-1 {{invalid conversion from throwing function of type '() throws -> Void' to non-throwing function type '() throws(Never) -> Void'}}

  let _: () throws(Never) -> Void = {
    throw MyError.fail // expected-error {{thrown expression type 'MyError' cannot be converted to error type 'Never'}}
  }

  let _: () throws(Never) -> Void = { () throws in
    // expected-error@-1 {{invalid conversion from throwing function of type '() throws -> Void' to non-throwing function type '() throws(Never) -> Void'}}
    throw MyError.fail
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

func testThrowsMismatch(fn: () throws -> Void) {
  func test(_: () throws(Never) -> Void) {}

  test(fn)
  // expected-error@-1 {{invalid conversion from throwing function of type '() throws -> Void' to non-throwing function type '() throws(Never) -> Void'}}
}

func testOverloadingWithConcreteErrorType() throws {
  func test<E>(_: () throws(E) -> Void) throws(E) {
  }

  struct S<T> {
    init(_: () throws -> T) throws {}
    init(_: () throws(MyError) -> T) throws(MyError) {}
  }

  struct Q {
    init() throws {}
  }

  try test {
    _ = try S {
      try Q() // Ok
    }
  }
}
