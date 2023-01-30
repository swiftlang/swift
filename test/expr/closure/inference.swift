// RUN: %target-typecheck-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Anonymous arguments with inference
func myMap<T, U>(_ array: [T], _ f: (T) -> U) -> [U] {}

func testMap(_ array: [Int]) {
  var farray = myMap(array, { Float($0) })
  var _ : Float = farray[0]
  let farray2 = myMap(array, { x in Float(x) })
  farray = farray2
  _ = farray
}

// Infer result type.
func testResultType() {
  takeIntToInt({x in
    return x + 1
  })

  takeIntIntToInt({x, y in
    return 2 + 3
  })
}

// Closures with unnamed parameters
func unnamed() {
  takeIntToInt({_ in return 1})
  takeIntIntToInt({_, _ in return 1})
}

// Regression tests.

var nestedClosuresWithBrokenInference = { f: Int in {} }
    // expected-error@-1 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
    // expected-error@-2 {{expected expression}}
    // expected-error@-3 {{cannot find 'f' in scope}}

// https://github.com/apple/swift/issues/53941
do {
  func f1<R>(action: () -> R) -> Void {}

  func f1<T, R>(action: (T) -> R) -> Void {}

  func f2<T, R>(action: (T) -> R) -> Void {}

  f1(action: { return }) // Ok f1<R>(action: () -> R) was the selected overload.

  // In case that's the only possible overload, it's acceptable
  f2(action: { return }) // OK
}

// https://github.com/apple/swift/issues/51081
do {
  func f1<A,Z>(_ f: @escaping (A) -> Z) -> (A) -> Z {}

  func f1<A,B,Z>(_ f: @escaping (A, B) -> Z) -> (A, B) -> Z {}

  let aa = f1 { (a: Int) in }
  let bb = f1 { (a1: Int, a2: String) in } // expected-note {{'bb' declared here}}

  aa(1) // Ok
  bb(1, "2") // Ok
  bb(1) // expected-error {{missing argument for parameter #2 in call}}

  // Tuple
  let cc = f1 { (_: (Int)) in }

  cc((1)) // Ok
  cc(1) // Ok
}

// https://github.com/apple/swift/issues/55401
do {
  let f: @convention(c) (T) -> Void // expected-error {{cannot find type 'T' in scope}}
}

// https://github.com/apple/swift/issues/42790
do {
  func foo<T>(block: () -> ()) -> T.Type { T.self } // expected-note {{in call to function 'foo(block:)'}}

  let x = foo { // expected-error {{generic parameter 'T' could not be inferred}}
    print("")
  }
}
