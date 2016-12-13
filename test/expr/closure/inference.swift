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
    // expected-error@-1 {{expression resolves to an unused function}}
    // expected-error@-2 {{consecutive statements on a line must be separated by ';'}} {{44-44=;}}
    // expected-error@-3 {{expected expression}}
    // expected-error@-4 {{use of unresolved identifier 'f'}}

// SR-1976/SR-3073: Inference of inout

func sr1976<T>(_ closure: (inout T) -> Void) {
}

sr1976({ $0 += 2 })

// SR-3073: UnresolvedDotExpr in single expression closure

func sr3073<S, T>(_ closure:(inout S, T) -> ()) {}

sr3073({ $0.number1 = $1 }) //expected-error {{type of expression is ambiguous without more context}}

struct SR3073Lense<Whole, Part> {
  let set: (inout Whole, Part) -> ()
}

struct SR3073 {
  var number1: Int
  
  func lenses() {
    let _: SR3073Lense<SR3073, Int> = SR3073Lense(
      set: { $0.number1 = $1 }
    )
  }
}
