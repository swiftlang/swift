// RUN: %swift -parse %s -verify

// Never inheritable: method with 'Self' in its signature
protocol P1 {
  func f1(x: Self) -> Bool
}

// Never inheritable: property with 'Self' in its signature.
protocol P2 {
  var prop2: Self { get }
}

// Never inheritable: subscript with 'Self' in its signature
protocol P3 {
  subscript (i: Int) -> Self { get }
}

// Never inheritable: subscript with 'Self' in its signature
protocol P4 {
  subscript (s: Self) -> Int { get }
}

// Potentially inheritable: method returning Self
protocol P5 {
  func f5() -> Self
}

// Inheritable: method returning DynamicSelf
protocol P6 {
  func f6() -> DynamicSelf
}

// Inheritable: method involving associated type.
protocol P7 {
  typealias Assoc
  func f7() -> Assoc
}

// Class A conforms to everything.
class A : P1, P2, P3, P4, P5, P6, P7 {
  // P1
  func f1(a: A) -> Bool { return true }

  // P2
  var prop2: A {
    return self
  }

  // P3
  subscript (i: Int) -> A {
  get:
    return self
  }
  
  // P4
  subscript (a: A) -> Int { 
  get: 
    return 5
  }

  // P5 
  func f5() -> A { return self }

  // P6 
  func f6() -> DynamicSelf { return self }

  // P7
  func f7() -> Int { return 5 }
}

// Class B inherits A; gets all of its inheritable conformances.
class B : A { }

func testB(b: B) {
  var p1: P1 = b // expected-error{{type 'B' does not conform to protocol 'P1'}}
  var p2: P2 = b // expected-error{{type 'B' does not conform to protocol 'P2'}}
  var p3: P3 = b // expected-error{{type 'B' does not conform to protocol 'P3'}}
  var p4: P4 = b // expected-error{{type 'B' does not conform to protocol 'P4'}}
  var p5: P5 = b // expected-error{{type 'B' does not conform to protocol 'P5'}}
  var p6: P6 = b
  var p7: P7 = b
}

// Class A5 conforms to P5 in an inheritable manner.
class A5 : P5 {
  // P5 
  func f5() -> DynamicSelf { return self }
}

// Class B5 inherits A5; gets all of its inheritable conformances.
class B5 : A5 { }

func testB5(b5: B5) {
  var p5: P5 = b5 // okay
}
