// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitLastExprResults

// Required for experimental features
// REQUIRES: asserts

func testMismatch1() -> Int {
  if .random() {
    print("hello")
    0
  } else {
    print("hi")
    "" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
  }
}

func testMismatch2() -> Int {
  let x = if .random() {
    print("hello")
    0 // expected-error {{branches have mismatching types 'Int' and 'String'}}
  } else {
    print("hi")
    ""
  }
  return x
}

func testVoidNeverConversion() {
  // We allow the T -> Void and Never -> T conversions, same as the single
  // expression case.
  let _: () -> Void = {
    if .random() {
      print("hello")
      1 // expected-warning {{integer literal is unused}}
    } else {
      print("there")
      2 // expected-warning {{integer literal is unused}}
    }
  }
  let _: () -> Void = {
    if .random() {
      print("hello")
      fatalError()
    } else {
      0 // expected-warning {{integer literal is unused}}
    }
  }
  // We fall back to Void if we have a mismatch.
  let _ = {
    if .random() {
      ()
      "" // expected-warning {{string literal is unused}}
    } else {
      ()
      0 // expected-warning {{integer literal is unused}}
    }
  }
  // Unless there's a contextual type.
  let _ = { () -> String in
    if .random() {
      ()
      ""
    } else {
      ()
      0 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
    }
  }
  let _: () -> String = {
    if .random() {
      ()
      ""
    } else {
      ()
      0 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
    }
  }
  let _ = {
    if .random() {
      ()
      ()
    } else {
      0 // expected-warning {{integer literal is unused}}
    }
  }
  let _ = {
    switch Bool.random() {
    case true:
      print("hello")
      ""
    case false:
      fatalError()
    }
  }
  func foo() -> Int {
    switch Bool.random() {
    case true:
      fatalError()
    case false:
      ()
      fatalError()
    }
  }
  func bar() -> Int {
    switch Bool.random() {
    case true:
      ()
      fatalError()
    case false:
      ()
      0
    }
  }
}

enum Either<T, U> {
  case first(T), second(U)
}

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildBlock<T, U>(_ x: T, _ y: U) -> (T, U) { (x, y) }

  static func buildEither<T, U>(first x: T) -> Either<T, U> { .first(x) }
  static func buildEither<T, U>(second x: U) -> Either<T, U> { .second(x) }

  static func buildExpression(_ x: Double) -> Double { x }
  static func buildExpression<T>(_ x: T) -> T { x }
}

// Implicit 'then' statements are transparent to the result builder transform.
@Builder
func testBuilder1() -> Either<(Void, Int), (Void, Int)> {
  if .random() {
    ()
    0
  } else {
    ()
    1
  }
}

@Builder
func testBuilder2() -> (Either<(Void, Int), String>, Void) {
  if .random() {
    ()
    0
  } else {
    ""
  }
  ()
}

@Builder
func testBuilder3() -> Either<Int, String> {
  // Bindings should still work though.
  let x = if .random() {
    print("hello")
    0
  } else {
    ()
    1
  }
  if .random() {
    x
  } else {
    ""
  }
}
