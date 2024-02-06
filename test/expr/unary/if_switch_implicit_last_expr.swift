// RUN: %target-typecheck-verify-swift -enable-experimental-feature ImplicitLastExprResults

// Required for experimental features
// REQUIRES: asserts

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
