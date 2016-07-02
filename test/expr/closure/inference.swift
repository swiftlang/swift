// RUN: %target-parse-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Anonymous arguments with inference
func myMap<T, U>(_ array: [T], _ f: (T) -> U) -> [U] {}

func testMap(_ array: [Int]) {
  var farray = myMap(array, { Float($0) }) // expected-note {{use trailing closure to simplify arguments}}
  var _ : Float = farray[0]
  let farray2 = myMap(array, { x in Float(x) }) // expected-note {{use trailing closure to simplify arguments}}
  farray = farray2
  _ = farray
}

// Infer result type.
func testResultType() {
  takeIntToInt({x in // expected-note {{use trailing closure to simplify arguments}}
    return x + 1
  })

  takeIntIntToInt({x, y in // expected-note {{use trailing closure to simplify arguments}}
    return 2 + 3
  })
}

// Closures with unnamed parameters
func unnamed() {
  takeIntToInt({_ in return 1}) // expected-note {{use trailing closure to simplify arguments}}
  takeIntIntToInt({_, _ in return 1}) // expected-note {{use trailing closure to simplify arguments}}
}

// Regression tests.

var nestedClosuresWithBrokenInference = { f: Int in {} }
    // expected-error@-1 {{expression resolves to an unused function}}
    // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
    // expected-error@-3 {{expected expression}}
    // expected-error@-4 {{use of unresolved identifier 'f'}}
