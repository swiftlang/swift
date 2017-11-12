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
    // expected-error@-1 {{closure expression is unused}} expected-note@-1 {{did you mean to use a 'do' statement?}} {{53-53=do }}
    // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
    // expected-error@-3 {{expected expression}}
    // expected-error@-4 {{use of unresolved identifier 'f'}}
