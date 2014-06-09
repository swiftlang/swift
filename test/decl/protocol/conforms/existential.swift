// RUN: %swift %s -verify

// Check for protocol conformance to itself within an existential.
protocol ConformsToSelf1 {
  func method() -> Int
  var property : String { get }
  subscript (i : Int) -> String { get }
}

func sc1<T : ConformsToSelf1>(x: T) { }

func testSelfConforming(c1 : ConformsToSelf1) {
  sc1(c1)
}

protocol NonSelfConforming1 {
  func isEqual(x: Self) -> Bool
}

protocol NonSelfConforming2 {
  var property : (Self, Int) { get }
}

protocol NonSelfConforming3 {
  subscript (i : Int) -> Self { get }
}

protocol NonSelfConforming4 {
  typealias Assoc
}

func nsc1<T : NonSelfConforming1>(x: T) { }
func nsc2<T : NonSelfConforming2>(x: T) { }
func nsc3<T : NonSelfConforming3>(x: T) { }
func nsc4<T : NonSelfConforming4>(x: T) { }

func testNonSelfConforming(c1: NonSelfConforming1, c2: NonSelfConforming2,
                           c3: NonSelfConforming3, c4: NonSelfConforming4) {
  nsc1(c1) // expected-error{{cannot convert the expression's type '()' to type 'T'}}
  nsc2(c2) // expected-error{{cannot convert the expression's type '()' to type 'T'}}
  nsc3(c3) // expected-error{{cannot convert the expression's type '()' to type 'T'}}
  nsc4(c4) // expected-error{{cannot convert the expression's type '()' to type 'T'}}
}
