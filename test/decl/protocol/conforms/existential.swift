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
  func isEqual(x: Self) -> Bool // expected-note {{'isEqual' requirement refers to 'Self' type}}
}

protocol NonSelfConforming2 {
  var property : (Self, Int) { get } // expected-note {{'_' requirement refers to 'Self' type}}
}

protocol NonSelfConforming3 {
  subscript (i : Int) -> Self { get } // expected-note {{'subscript' requirement refers to 'Self' type}}
}

protocol NonSelfConforming4 {
  typealias Assoc // expected-note {{associated type 'Assoc' prevents protocol from conforming to itself}}
}

func nsc1<T : NonSelfConforming1>(x: T) { }
func nsc2<T : NonSelfConforming2>(x: T) { }
func nsc3<T : NonSelfConforming3>(x: T) { }
func nsc4<T : NonSelfConforming4>(x: T) { }

func testNonSelfConforming(c1: NonSelfConforming1, c2: NonSelfConforming2,
                           c3: NonSelfConforming3, c4: NonSelfConforming4) {
  nsc1(c1) // expected-error{{type 'NonSelfConforming1' does not conform to protocol 'NonSelfConforming1'}}
  nsc2(c2) // expected-error{{type 'NonSelfConforming2' does not conform to protocol 'NonSelfConforming2'}}
  nsc3(c3) // expected-error{{type 'NonSelfConforming3' does not conform to protocol 'NonSelfConforming3'}}
  nsc4(c4) // expected-error{{type 'NonSelfConforming4' does not conform to protocol 'NonSelfConforming4'}}
}
