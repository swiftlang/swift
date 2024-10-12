// RUN: %target-typecheck-verify-swift -enable-experimental-feature ThenStatements

// REQUIRES: swift_feature_ThenStatements

enum E {
  case e
  case f
  case g(Int)
}

func testVoidNeverConversion() {
  // Explicit 'then' statements don't participate in implicit Void/Never conversions.
  let _: () -> Void = {
    if .random() {
      then 0 // expected-error {{cannot convert value of type 'Int' to specified type 'Void'}}
    } else {
      0
    }
  }
  let _: () -> Void = {
    if .random() {
      then fatalError() // expected-error {{cannot convert value of type 'Never' to specified type 'Void'}}
    } else {
      0
    }
  }
  let _ = {
    if .random() {
      then "" // expected-error {{branches have mismatching types 'String' and 'Int'}}
    } else {
      then 0
    }
  }
  let _ = {
    if .random() {
      then ()
    } else {
      0 // expected-warning {{integer literal is unused}}
    }
  }
  let _ = {
    if .random() {
      then ""
    } else {
      fatalError()
    }
  }
  func foo() -> Int {
    switch Bool.random() {
    case true:
      fatalError()
    case false:
      then fatalError() // expected-error {{cannot convert value of type 'Never' to specified type 'Int'}}
    }
  }
}

enum Either<T, U> {
  case first(T), second(U)
}

@resultBuilder
struct Builder { // expected-note 2{{struct 'Builder' declared here}}
  static func buildBlock<T>(_ x: T) -> T { x }
  static func buildBlock<T, U>(_ x: T, _ y: U) -> (T, U) { (x, y) }

  static func buildEither<T, U>(first x: T) -> Either<T, U> { .first(x) }
  static func buildEither<T, U>(second x: U) -> Either<T, U> { .second(x) }

  static func buildExpression(_ x: Double) -> Double { x }
  static func buildExpression<T>(_ x: T) -> T { x }
}

@Builder
func testThenStmtBuilder1() -> Either<Int, Int> {
  // We don't allow explicit 'then' to be part of the transform.
  if .random() {
    then 0 // expected-error {{closure containing control flow statement cannot be used with result builder 'Builder'}}
  } else {
    1
  }
}

@Builder
func testThenStmtBuilder2() -> Either<Int, Int> {
  // We don't allow explicit 'then' to be part of the transform.
  if .random() {
    then 0 // expected-error {{closure containing control flow statement cannot be used with result builder 'Builder'}}
  } else {
    1
  }
  ()
}

@Builder
func testThenStmtBuilder3() -> Either<Int, String> {
  // But it's fine as part of e.g a binding.
  let x = if .random() {
    then 0
  } else {
    1
  }
  if .random() {
    x
  } else {
    ""
  }
}
