// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitLastExprResults -disable-availability-checking

// REQUIRES: swift_feature_ImplicitLastExprResults

let a = if .random() {
  print("hello")
  6
} else {
  7
}

let b = if .random() {
  print("hello")
  if .random() { 5 } else { 6 }
} else {
  let x = 7
  x
}

let c = switch Bool.random() {
case true:
  print("hello")
  6
case false:
  ()
  7
}

let d = switch Bool.random() {
case true:
  print("hello")
  if .random() { 5 } else { 6 }
case false:
  7
}

// TODO: This error message will need updating if we enable this feature by default.
let e = if .random() {
  ()
  let _ = 0
} else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  0
}
let f = switch Bool.random() {
case true:
  ()
  let _ = 0 // expected-error {{non-expression branch of 'switch' expression may only end with a 'throw'}}
case false:
  0
}

func testFn1() -> Int {
  print("hello")
  0
}

func testFn2() -> Int {
  print("hello")
  if .random() { 0 } else { 0 }
}

func testFn3() -> Int {
  print("hello")
  switch Bool.random() {
  case true:
    print("hello")
    if .random() {
      0
    } else {
      print("hello")
      0
    }
  case false:
    0
  }
}

func testFn4() {
  print("hello")
  1 // expected-warning {{integer literal is unused}}
}

var testComputedProp: String {
  print("hello")
  "there"
}

protocol P {}

struct S: P {
  subscript() -> Int {
    ()
    0
  }
  subscript(x: Int) -> Int {
    get {
      ()
      x
    }
    set {
      ()
      x // expected-warning {{expression of type 'Int' is unused}}
    }
  }
}

func testOpaqueReturn() -> some P {
  print("hello")
  S()
}

func takesFn(_ fn: () -> Int) {}

func testClosure1() {
  takesFn {
    ()
    0
  }
}

func testClosure2() {
  let fn = {
    ()
    if .random() { 0 } else { 1 }
  }
  takesFn(fn)
}

func testClosure3() {
  let fn = {
    switch Bool.random() {
    case true:
      ()
      0
    case false:
      ()
      1
    }
  }
  takesFn(fn)
}

func testClosure4() {
  let fn = {
    ()
    switch Bool.random() {
    case true:
      ()
      0
    case false:
      ()
      1
    }
  }
  takesFn(fn)
}

func testClosure5() {
  let fn = {
    ()
    switch Bool.random() {
    case true:
      ()
      0
    case false:
      fatalError()
    }
  }
  takesFn(fn)
}

func testClosure6() {
  let fn = {
    ()
    switch Bool.random() {
    case true:
      ()
      if .random() {
        print("hello")
        0
      } else {
        0
      }
    case false:
      fatalError()
    }
  }
  takesFn(fn)
}

func testClosure7() {
  let fn = {
    ()
    switch Bool.random() {
    case true:
      fatalError()
    case false:
      ()
      if .random() {
        print("hello")
        0
      } else {
        0
      }
    }
  }
  takesFn(fn)
}

func testMismatch1() -> Int {
  print("hi")
  "" // expected-error {{cannot convert return expression of type 'String' to return type 'Int'}}
}

func testMismatch2() -> Int {
  if .random() {
    print("hello")
    0
  } else {
    print("hi")
    "" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
  }
}

func testMismatch3() -> Int {
  print("hello")
  if .random() {
    print("hello")
    0
  } else {
    print("hi")
    "" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
  }
}

func testMismatch4() -> Int {
  let x = if .random() {
    print("hello")
    0 // expected-error {{branches have mismatching types 'Int' and 'String'}}
  } else {
    print("hi")
    ""
  }
  return x
}

func testMismatch5() {
  takesFn {
    ()
    "" // expected-error {{cannot convert value of type 'String' to closure result type 'Int'}}
  }
}

func testMismatch6() {
  takesFn {
    ()
    if .random() {
      ()
      ""  // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
    } else {
      ()
      ""
    }
  }
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
    print("hello")
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
  let _: () -> Void = {
    print("hello")
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
  let _ = {
    print("hello")
    if .random() {
      ()
      "" // expected-warning {{string literal is unused}}
    } else {
      ()
      0 // expected-warning {{integer literal is unused}}
    }
  }
  let _ = {
    print("hello")
    if .random() {
      ()
      switch Bool.random() { case true: (); "" case false: 0 }
      // expected-warning@-1 {{string literal is unused}}
      // expected-warning@-2 {{integer literal is unused}}
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
  let _ = { () -> String in
    print("hello")
    if .random() {
      ()
      ""
    } else {
      ()
      0 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
    }
  }
  let _: () -> String = {
    print("hello")
    if .random() {
      ()
      ""
    } else {
      ()
      0 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
    }
  }
  let _: () -> Void = {
    ()
    switch Bool.random() {
    case true:
      if .random() {
        switch Bool.random() {
        case true:
          ()
          0 // expected-warning {{integer literal is unused}}
        case false:
          fatalError()
        }
      } else {
        ()
      }
    case false:
      0 // expected-warning {{integer literal is unused}}
    }
  }
  // Doesn't apply if we have an explicit return.
  let _: () -> Void = {
    ()
    return if .random() { (); 1 } else { 0 }
    // expected-error@-1 {{cannot convert value of type 'Int' to specified type 'Void'}}
  }
  let _: () -> Void = {
    if .random() {
      return switch Bool.random() {
      case true:
        1 // expected-error {{cannot convert value of type 'Int' to specified type 'Void'}}
      case false:
        0
      }
    }
    if .random() { 0 } else { 1 }
  }
  let _: () -> Void = {
    if .random() {
      return fatalError() // expected-error {{cannot convert value of type 'Never' to closure result type 'Void'}}
    }
    if .random() { 0 } else { 1 }
  }
  // This is okay, while the return is explicit, the branches are implicit results.
  let _: () -> Void = {
    if .random() {
      return if .random() { fatalError() } else { fatalError() }
    }
    if .random() { 0 } else { 1 } // expected-warning 2{{integer literal is unused}}
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
func testBuilder2() -> (Void, Either<(Void, Int), (Void, Int)>) {
  print("hello")
  if .random() {
    ()
    0
  } else {
    ()
    1
  }
}

@Builder
func testBuilder3() -> (Either<(Void, Int), String>, Void) {
  if .random() {
    ()
    0
  } else {
    ""
  }
  ()
}

@Builder
func testBuilder4() -> Either<Int, String> {
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

// Okay, these won't become expressions.
func testBindingAsLast1() -> Int {
  if .random() {
    ()
    let _ = 0
  } else {
    0 // expected-warning {{integer literal is unused}}
  }
}
func testBindingAsLast2() -> Int {
  switch Bool.random() {
  case true:
    ()
    let _ = 0
  case false:
    0 // expected-warning {{integer literal is unused}}
  }
}

func testGuard1() -> Int {
  let x = if .random() {
    let x: Int? = nil
    guard let _ = x else { fatalError() }
  } else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
    0
  }
  return x
}

func testGuard2() -> Int {
  let x = switch Bool.random() {
  case true:
    let x: Int? = nil
    guard let _ = x else { fatalError() } // expected-error {{non-expression branch of 'switch' expression may only end with a 'throw'}}
  case false:
    0
  }
  return x
}

func testGuard3() -> Int {
  let x = if .random() {
    let x: Int? = nil
    guard let y = x else { fatalError() }
    y
  } else {
    0
  }
  return x
}

func testGuard4() -> Int {
  switch Bool.random() {
  case true:
    let x: Int? = nil
    guard let y = x else { fatalError() }
    y
  case false:
    0
  }
}

func testNested1() -> Int {
  if .random() {
    let a = 1
    if .random() {
      let b = 2
      a + b
    } else {
      let c = 3
      a + c
    }
  } else {
    let a = 1
    switch Bool.random() {
    case true:
      let b = 2
      a + b
    case false:
      1
    }
  }
}

func testNested2() -> Int {
  if .random() {
    if .random() { "" } else { "" } // expected-warning 2{{string literal is unused}}
    if .random() {
      ()
      1
    } else {
      ()
      2
    }
  } else {
    ()
    switch Bool.random() {
    case true:
      ()
      0
    case false:
      ()
      1
    }
  }
}

func testNested3() -> Int {
  let x = if .random() {
    if .random() {
      ()
      1
    } else {
      ()
      2
    }
  } else {
    switch Bool.random() {
    case true:
      ()
      0
    case false:
      ()
      1
    }
  }
  return x
}

func testPoundIf1() -> Int {
  ()
  if .random() {
    ()
    #if true
      ()
      0
    #else
      ""
    #endif
  } else {
    "" // expected-warning {{string literal is unused}}
    #if true
    0
    #endif
  }
}

func testPoundIf2() -> Int {
  ()
#if true
  ()
  0
#else
  ""
#endif
}

func nestedType1() -> Int {
  let x = if .random() {
    struct S {
      var x: Int
    }
    S(x: 0).x
  } else {
    1
  }
  return x
}

func nestedType2() -> Int {
  if .random() {
    struct S {
      var x: Int
    }
    S(x: 0).x
  } else {
    1
  }
}
