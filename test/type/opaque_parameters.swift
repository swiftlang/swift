// RUN: %target-typecheck-verify-swift -enable-experimental-opaque-parameters -disable-availability-checking

protocol P { }

protocol Q {
  associatedtype A: P & Equatable

  func f() -> A
}

extension Int: P { }
extension String: P { }

// expected-note@+1{{requirement from conditional conformance of '[Double]' to 'Q'}}
extension Array: Q where Element: P, Element: Equatable {
  func f() -> Element {
    return first!
  }
}

extension Set: Q where Element: P, Element: Equatable {
  func f() -> Element {
    return first!
  }
}

// expected-note@+2{{where 'some Q' = 'Int'}}
// expected-note@+1{{in call to function 'takesQ'}}
func takesQ(_ q: some Q) -> Bool {
  return q.f() == q.f()
}

func testTakesQ(arrayOfInts: [Int], setOfStrings: Set<String>, i: Int) {
  _ = takesQ(arrayOfInts)
  _ = takesQ(setOfStrings)
  _ = takesQ(i) // expected-error{{global function 'takesQ' requires that 'Int' conform to 'Q'}}

  let f = takesQ // expected-error{{generic parameter 'some Q' could not be inferred}}
  let _: ([String]) -> Bool = takesQ
  let _: ([Double]) -> Bool = takesQ // expected-error{{global function 'takesQ' requires that 'Double' conform to 'P'}}
  _ = f
}

// expected-note@+1{{where 'some P' = '[Int]'}}
func takeMultiple<T>(_: T, _: some Q, _: some P) { }

func testTakeMultiple(
  arrayOfInts: [Int], setOfStrings: Set<String>, i: Int, d: Double
) {
  takeMultiple(d, arrayOfInts, i)
  takeMultiple(d, arrayOfInts, arrayOfInts) // expected-error{{global function 'takeMultiple' requires that '[Int]' conform to 'P'}}
}
