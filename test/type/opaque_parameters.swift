// RUN: %target-typecheck-verify-swift -disable-availability-checking -requirement-machine-inferred-signatures=on

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

extension Set: Q where Element: P, Element: Equatable { // expected-warning {{redundant conformance constraint 'Element' : 'Equatable'}}
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

// inout

func anyInOut(_: inout some P) { }

func testAnyInOut() {
  var i = 17
  anyInOut(&i)
}

// In structural positions.
func anyDictionary(_ dict: [some Hashable: some Any]) { }

func testAnyDictionary(numberNames: [Int: String]) {
  anyDictionary(numberNames)
}

// Combine with parameterized protocol types
protocol PrimaryCollection<Element>: Collection {}

extension Array: PrimaryCollection { }
extension Set: PrimaryCollection { }

func takePrimaryCollections(
  _ strings: some PrimaryCollection<String>,
  _ ints : some PrimaryCollection<Int>
) {
  for s in strings {
    let _: String = s
  }

  for i in ints {
    let _: Int = i
  }
}

func takeMatchedPrimaryCollections<T: Equatable>(
  _ first: some PrimaryCollection<T>, _ second: some PrimaryCollection<T>
) -> Bool {
  first.elementsEqual(second)
}

func testPrimaries(
  arrayOfInts: [Int], setOfStrings: Set<String>, setOfInts: Set<Int>
) {
  takePrimaryCollections(setOfStrings, setOfInts)
  takePrimaryCollections(setOfStrings, arrayOfInts)
  _ = takeMatchedPrimaryCollections(arrayOfInts, setOfInts)
  _ = takeMatchedPrimaryCollections(arrayOfInts, setOfStrings) // expected-error{{type of expression is ambiguous without more context}}
}


// Prohibit use of opaque parameters in consuming positions.
typealias FnType<T> = (T) -> Void

func consumingA(fn: (some P) -> Void) { } // expected-error{{'some' cannot appear in parameter position in parameter type '(some P) -> Void'}}
func consumingB(fn: FnType<some P>) { } // expected-error{{'some' cannot appear in parameter position in parameter type '(some P) -> Void'}}
