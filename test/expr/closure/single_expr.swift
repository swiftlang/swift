// RUN: %target-parse-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Simple closures with anonymous arguments
func simple() {
  takeIntToInt({$0 + 1})
  takeIntIntToInt({$0 + $1 + 1})
}

// Anonymous arguments with inference
func myMap<T, U>(_ array: [T], _ f: (T) -> U) -> [U] {}

func testMap(_ array: [Int]) {
  var farray = myMap(array, { Float($0) })
  var _ : Float = farray[0]
  let farray2 = myMap(array, { (x : Int) in Float(x) })
  farray = farray2
  _ = farray
}

// Nested single-expression closures -- <rdar://problem/20931915>
class NestedSingleExpr {
  private var b: Bool = false
  private func callClosure(_ callback: (Void) -> Void) {}

  func call() {
    callClosure { [weak self] in
      self?.callClosure {
        self?.b = true
      }
    }
  }
}

// Autoclosure nested inside single-expr closure should get discriminator
// <rdar://problem/22441425> Swift compiler "INTERNAL ERROR: this diagnostic should not be produced"
struct Expectation<T> {}
func expect<T>(_ expression: @autoclosure () -> T) -> Expectation<T> {
  return Expectation<T>()
}
func describe(_ closure: () -> ()) {}
func f() { describe { _ = expect("what") } }
