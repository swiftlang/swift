// RUN: %target-parse-verify-swift

// Inheritable: method with 'Self' in its signature
protocol P1 {
  func f1(x: Self?) -> Bool
}

// Never inheritable: property with 'Self' in its signature.
protocol P2 {
  var prop2: Self { get }
}

// Never inheritable: subscript with 'Self' in its result type.
protocol P3 {
  subscript (i: Int) -> Self { get }
}

// Inheritable: subscript with 'Self' in its index type.
protocol P4 {
  subscript (s: Self) -> Int { get }
}

// Potentially inheritable: method returning Self
protocol P5 {
  func f5() -> Self
}

// Inheritable: method returning Self
protocol P6 {
  func f6() -> Self
}

// Inheritable: method involving associated type.
protocol P7 {
  typealias Assoc
  func f7() -> Assoc
}

// Inheritable: initializer requirement.
protocol P8 {
  init(int: Int)
}

// Inheritable: operator requirement.
protocol P9 {
  func ==(x: Self, y: Self) -> Bool 
}

// Never inheritable: method with 'Self' in a non-contravariant position.
protocol P10 {
  func f10(arr: [Self])
}

// Never inheritable: method with 'Self' in curried position.
protocol P11 {
  func f11()(x: Self) -> Int
}


// Class A conforms to everything that can be conformed to by a
// non-final class.
class A : P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 {
  // P1
  func f1(x: A?) -> Bool { return true }

  // P2
  var prop2: A { // expected-error{{protocol 'P2' requirement 'prop2' cannot be satisfied by a non-final class ('A') because it uses 'Self' in a non-parameter, non-result type position}}
    return self
  }

  // P3
  subscript (i: Int) -> A { // expected-error{{protocol 'P3' requirement 'subscript' cannot be satisfied by a non-final class ('A') because it uses 'Self' in a non-parameter, non-result type position}}
    get {
     return self
    }
  }

  // P4
  subscript (a: A) -> Int { 
    get {
      return 5
    }
  }

  // P5
  func f5() -> A { return self } // expected-error{{method 'f5()' in non-final class 'A' must return `Self` to conform to protocol 'P5'}}

  // P6
  func f6() -> Self { return self }

  // P7
  func f7() -> Int { return 5 }

  // P8
  required init(int: Int) { }

  // P10
  func f10(arr: [A]) { } // expected-error{{protocol 'P10' requirement 'f10' cannot be satisfied by a non-final class ('A') because it uses 'Self' in a non-parameter, non-result type position}}

  // P11
  func f11()(x: A) -> Int { return 5 }
}

// P9
func ==(x: A, y: A) -> Bool { return true }

// Class B inherits A; gets all of its conformances.
class B : A { 
  required init(int: Int) { }
}

func testB(b: B) {
  var p1: P1 = b // expected-error{{has Self or associated type requirements}}
  var p4: P4 = b // expected-error{{has Self or associated type requirements}}
  var p5: P5 = b
  var p6: P6 = b
  var p7: P7 = b // expected-error{{has Self or associated type requirements}}
  var p8: P8 = b // okay
  var p9: P9 = b // expected-error{{has Self or associated type requirements}}
}

// Class A5 conforms to P5 in an inheritable manner.
class A5 : P5 {
  // P5 
  func f5() -> Self { return self }
}

// Class B5 inherits A5; gets all of its conformances.
class B5 : A5 { }

func testB5(b5: B5) {
  var p5: P5 = b5 // okay
}

// Class A8 conforms to P8 in an inheritable manner.
class A8 : P8 {
  required init(int: Int) { }
}

class B8 : A8 {
  required init(int: Int) { }
}

func testB8(b8: B8) {
  var p8: P8 = b8 // okay
}

// Class A9 conforms to everything.
final class A9 : P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 {
  // P1
  func f1(x: A9?) -> Bool { return true }

  // P2
  var prop2: A9 {
    return self
  }

  // P3
  subscript (i: Int) -> A9 {
    get {
     return self
    }
  }

  // P4
  subscript (a: A9) -> Int { 
    get {
      return 5
    }
  }

  // P5
  func f5() -> A9 { return self }

  // P6
  func f6() -> Self { return self }

  // P7
  func f7() -> Int { return 5 }

  // P8
  required init(int: Int) { }

  // P10
  func f10(arr: [A9]) { }

  // P11
  func f11()(x: A9) -> Int { return 5 }
}

// P9
func ==(x: A9, y: A9) -> Bool { return true }
