// RUN: %target-parse-verify-swift

class C1 { }

protocol WeakP1 {
  weak var property: C1? { get } // expected-note{{protocol requires property 'property' with type 'C1?'}}
}

struct WeakS1a : WeakP1 {
  weak var property: C1? // okay
}

struct WeakS1b : WeakP1 { // expected-error{{type 'WeakS1b' does not conform to protocol 'WeakP1'}}
  var property: C1? // expected-note{{candidate has strong ownership, not 'weak' as required}}
}

protocol UnownedP1 {
  unowned var property: C1 { get } // expected-note{{protocol requires property 'property' with type 'C1'}}
}

struct UnownedS1a : UnownedP1 {
  unowned var property: C1 // okay
}

struct UnownedS1b : UnownedP1 {  // expected-error{{type 'UnownedS1b' does not conform to protocol 'UnownedP1'}}
  var property: C1 // expected-note{{candidate has strong ownership, not 'unowned' as required}}
}

protocol StrongP1 {
  var property: C1 { get } // expected-note{{protocol requires property 'property' with type 'C1'}}
}

struct StrongS1a : StrongP1 {
  var property: C1 // okay
}

struct StrongS1b : StrongP1 { // expected-error{{type 'StrongS1b' does not conform to protocol 'StrongP1'}}
  unowned var property: C1 // expected-note{{candidate has 'unowned' ownership, not strong as required}}
}
