// RUN: %target-typecheck-verify-swift -enable-experimental-feature DoExpressions -enable-experimental-feature ImplicitLastExprResults
// RUN: %target-swift-emit-ir -enable-experimental-feature DoExpressions -enable-experimental-feature ImplicitLastExprResults %s

// Required for experimental features
// REQUIRES: asserts

let a = do {
  print("hello")
  6
}

let b = do {
  print("hello")
  if .random() { 5 } else { 6 }
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

func throwingFn() throws {}

func doCatch1() -> Int {
  do {
    try throwingFn()
    5
  } catch {
    print("\(error)")
    0
  }
}
