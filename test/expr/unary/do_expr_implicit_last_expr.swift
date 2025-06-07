// RUN: %target-typecheck-verify-swift -enable-experimental-feature DoExpressions -enable-experimental-feature ImplicitLastExprResults
// RUN: %target-swift-emit-ir -enable-experimental-feature DoExpressions -enable-experimental-feature ImplicitLastExprResults %s

// REQUIRES: swift_feature_DoExpressions
// REQUIRES: swift_feature_ImplicitLastExprResults

let a = do {
  print("hello")
  6
}

let b = do {
  print("hello")
  if .random() { 5 } else { 6 }
}

let c = do {
  print("hello")
  if .random() {
    print("hello")
    5
  } else {
    6
  }
}

func throwingFn() throws {}

func testFn() -> Int {
  print("hello")
  do {
    try throwingFn()
    0
  } catch {
    print("error")
    fatalError()
  }
}

func testClosure() -> Int {
  let fn = {
    print("hello")
    do {
      try throwingFn()
      0
    } catch {
      print("error")
      fatalError()
    }
  }
  return fn()
}

func nestedType1() -> Int {
  let x = do {
    struct S {
      var x: Int
    }
    S(x: 0).x
  }
  return x
}

func nestedType2() -> Int {
  do {
    struct S {
      var x: Int
    }
    S(x: 0).x
  }
}

func doCatch1() -> Int {
  do {
    try throwingFn()
    5
  } catch {
    print("\(error)")
    0
  }
}
