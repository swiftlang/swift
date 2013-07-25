// RUN: %swift %s -verify

// Check for protocol conformance to itself within an existential.
protocol ConformsToSelf1 {
  func method() -> Int
  var property : String
  subscript (i : Int) -> String
}

func sc1<T : ConformsToSelf1>(x : T) { }

func testSelfConforming(c1 : ConformsToSelf1) {
  sc1(c1)
}

protocol NonSelfConforming1 {
  func isEqual(x : This) -> Bool
}

protocol NonSelfConforming2 {
  var property : (This, Int)
}

protocol NonSelfConforming3 {
  subscript (i : Int) -> This
}

protocol NonSelfConforming4 {
  typealias Assoc
}

func nsc1<T : NonSelfConforming1>(x : T) { }
func nsc2<T : NonSelfConforming2>(x : T) { }
func nsc3<T : NonSelfConforming3>(x : T) { }
func nsc4<T : NonSelfConforming4>(x : T) { }

func testNonSelfConforming(c1 : NonSelfConforming1, c2 : NonSelfConforming2,
                           c3 : NonSelfConforming3, c4 : NonSelfConforming4) {
  nsc1(c1) // expected-error{{does not type-check}}
  nsc2(c2) // expected-error{{does not type-check}}
  nsc3(c3) // expected-error{{does not type-check}}
  nsc4(c4) // expected-error{{does not type-check}}
}
