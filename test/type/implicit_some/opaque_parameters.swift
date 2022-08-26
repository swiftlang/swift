// RUN: %target-typecheck-verify-swift -disable-availability-checking -warn-redundant-requirements  -enable-experimental-implicit-some

protocol P { }

protocol Q {
  associatedtype A: P & Equatable

  func f() -> A
  func takesA(_: A)
}

extension Int: P { }
extension String: P { }

// expected-note@+1{{requirement from conditional conformance of '[Double]' to 'Q'}}
extension Array: Q where Element: P, Element: Equatable {
  func f() -> Element {
    return first!
  }

  func takesA(_: Element) {}
}

extension Set: Q where Element: P, Element: Equatable { // expected-warning {{redundant conformance constraint 'Element' : 'Equatable'}}
  func f() -> Element {
    return first!
  }

  func takesA(_: Element) {}
}

// expected-note@+2{{where 'Q' = 'Int'}}
// expected-note@+1{{in call to function 'takesImplicitQ'}}
func takesImplicitQ(_ q: Q) -> Bool {
  // expected-error@+1 {{cannot convert value of type 'Int' to expected argument type '(Q).A'}}
  q.takesA(1)

  return q.f() == q.f()
}

func testTakesImplicitQ(arrayOfInts: [Int], setOfStrings: Set<String>, i: Int) {
  _ = takesImplicitQ(arrayOfInts)
  _ = takesImplicitQ(setOfStrings)
  _ = takesImplicitQ(i) // expected-error{{global function 'takesImplicitQ' requires that 'Int' conform to 'Q'}}

  let f = takesImplicitQ // expected-error{{generic parameter 'Q' could not be inferred}}
  let _: ([String]) -> Bool = takesImplicitQ
  let _: ([Double]) -> Bool = takesImplicitQ // expected-error{{global function 'takesImplicitQ' requires that 'Double' conform to 'P'}}
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
func implicitInOut(_: inout P) { }

func testInOut() {
  var i = 17
  implicitInOut(&i)
}

// Combine with parameterized protocol types
protocol PrimaryCollection<Element>: Collection {}

extension Array: PrimaryCollection { }
extension Set: PrimaryCollection { }

func takePrimaryCollections(
  _ strings: PrimaryCollection<String>,
  _ ints : PrimaryCollection<Int>
) {
  for s in strings {
    let _: String = s
  }

  for i in ints {
    let _: Int = i
  }
}

func takeMatchedPrimaryCollections<T: Equatable>(
  _ first: PrimaryCollection<T>, _ second: PrimaryCollection<T>
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

func consumingA(fn: (P) -> Void) { } // expected-error{{'some' cannot appear in parameter position in parameter type '(P) -> Void'}}
func consumingB(fn: FnType<P>) { } // expected-error{{'some' cannot appear in parameter position in parameter type '(P) -> Void'}}
