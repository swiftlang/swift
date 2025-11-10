// RUN: %target-typecheck-verify-swift

struct MyError: Error {}
enum MyNever {}

func never_throws() throws -> Never { throw MyError() }
func uninhabited_throws() throws -> (Int, MyNever) { throw MyError () }
func almost_uninhabited_throws() throws -> (Int, Never?) { throw MyError () }
func int_throws() throws -> Int { throw MyError() }
func void_throws() throws {}

func maybe_never() -> Never? { nil }
func maybe_uninhabited() -> (Int, MyNever)? { nil }
func maybe_maybe_uninhabited() -> (Int, Never)?? { nil }
func maybe_void() -> Void? { nil }
func maybe_maybe_void() -> Void?? { nil }
func looks_uninhabited_if_you_squint() -> (Int, Never?)? { nil }

// MARK: - Tests

func test_try() throws {
  try never_throws()
  try uninhabited_throws()
  try almost_uninhabited_throws()
  // expected-warning @-1 {{result of call to 'almost_uninhabited_throws()' is unused}}
  try int_throws()
  // expected-warning @-1 {{result of call to 'int_throws()' is unused}}
  try void_throws()
}

func test_force_try() throws {
  try! never_throws()
  try! uninhabited_throws()
  try! almost_uninhabited_throws()
  // expected-warning @-1 {{result of call to 'almost_uninhabited_throws()' is unused}}
  try! int_throws()
  // expected-warning @-1 {{result of call to 'int_throws()' is unused}}
  try! void_throws()
}

func test_optional_try() throws {
  try? never_throws()
  try? uninhabited_throws()
  try? almost_uninhabited_throws()
  // expected-warning @-1 {{result of 'try?' is unused}}
  try? int_throws()
  // expected-warning @-1 {{result of 'try?' is unused}}
  try? void_throws()
}

func test_implicitly_discardable() {
  maybe_never()
  maybe_uninhabited()
  maybe_maybe_uninhabited()
  maybe_void()
  maybe_maybe_void()
  looks_uninhabited_if_you_squint()
  // expected-warning @-1 {{result of call to 'looks_uninhabited_if_you_squint()' is unused}}
}

// https://github.com/swiftlang/swift/issues/85092

func test_85092() {
  struct MyError: Error {}

  func f() throws -> Never {
    throw MyError()
  }

  func g() {
    try? f()
  }
}
