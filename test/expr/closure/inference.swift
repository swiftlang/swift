// RUN: %target-parse-verify-swift

func takeIntToInt(f: (Int) -> Int) { }
func takeIntIntToInt(f: (Int, Int) -> Int) { }

// Anonymous arguments with inference
func myMap<T, U>(array: [T], f: (T) -> U) -> [U] {}

func testMap(array: [Int]) {
  var farray = myMap(array, { Float($0) })
  var f : Float = farray[0]
  var farray2 = myMap(array, { x in Float(x) })
  farray = farray2
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
    // expected-error@-1 {{unable to infer closure type in the current context}}
    // expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
    // expected-error@-3 {{expected expression}}
