// Matching rules for `@cxx @implementation` of C++ operators. A member
// operator is implemented by a method of the record, named freely and matched
// by the C++ name given in `@cxx(...)`; a free operator by a global function,
// named the same way or a Swift operator function of the same spelling, whose
// bare `@cxx` names the C++ operator.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Operators


// Member operators: `self` is `this` and `const` is non-`mutating`, as for any
// method; a reference parameter is spelled as the importer projects it and
// makes the implementation `@unsafe`.

extension Vector {
  @unsafe @cxx(`operator==`) @implementation
  func equals(_ other: Vector) -> Bool { return x == other.x }

  @unsafe @cxx(`operator<`) @implementation
  func less(_ other: Vector) -> Bool { return x < other.x }

  // The parameter types select the overload.
  @unsafe @cxx(`operator+`) @implementation
  func plus(_ other: Vector) -> Vector { return Vector(x: x + other.x) }

  @cxx(`operator+`) @implementation
  func plus(_ k: Int32) -> Vector { return Vector(x: x + k) }

  // So does the arity.
  @cxx(`operator-`) @implementation
  func negated() -> Vector { return Vector(x: -x) }

  @unsafe @cxx(`operator-`) @implementation
  func minus(_ other: Vector) -> Vector { return Vector(x: x - other.x) }

  // The importer drops the `Vector &` result of a compound assignment, but
  // the C++ caller expects it: the implementation returns it, as a pointer.
  @unsafe @cxx(`operator+=`) @implementation
  mutating func plusEquals(_ other: Vector) -> UnsafeMutablePointer<Vector> {
    x += other.x
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  @cxx(`operator[]`) @implementation
  func element(_ i: Int32) -> Int32 { return x + i }

  @cxx(`operator()`) @implementation
  func call(_ i: Int32) -> Int32 { return x * i }

  @unsafe @cxx(`operator++`) @implementation
  mutating func increment() -> UnsafeMutablePointer<Vector> {
    x += 1
    return withUnsafeMutablePointer(to: &self) { $0 }
  }

  // The postfix form is told apart by its `int` parameter.
  @cxx(`operator++`) @implementation
  mutating func postIncrement(_: Int32) -> Vector {
    let old = self
    x += 1
    return old
  }
}

// Free operators are implemented at the top level, wherever C++ scopes them.

@unsafe @cxx @implementation
func != (a: Vector, b: Vector) -> Bool { return a.x != b.x }

@unsafe @cxx(`operator*`) @implementation
func times(_ a: Vector, _ k: Int32) -> Vector { return Vector(x: a.x * k) }

@unsafe @cxx @implementation
func == (a: Outer.Point, b: Outer.Point) -> Bool { return a.v == b.v }

// Operators of a foreign reference type match like its methods.

@available(SwiftStdlib 5.8, *)
extension Handle {
  @unsafe @cxx(`operator==`) @implementation
  func equals(_ other: Handle) -> Bool { return value == other.value }

  // A reference to a foreign reference type is the type itself, and a
  // compound assignment returns it unretained.
  // expected-error@+2{{instance method 'plusEquals' cannot implement C++ function 'operator+=' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
  @unsafe @cxx(`operator+=`) @implementation
  func plusEquals(_ k: Int32) -> Handle { value += k; return self }
}

// Rejections

extension Defined {
  // expected-error@+2{{instance method 'equals' cannot implement C++ function 'operator==' because it already has a definition}}
  @unsafe @cxx(`operator==`) @implementation
  func equals(_ other: Defined) -> Bool { return true }

  // expected-error@+2{{instance method 'less' cannot implement C++ function 'operator<' because it is declared 'inline'}}
  @unsafe @cxx(`operator<`) @implementation
  func less(_ other: Defined) -> Bool { return true }
}

extension Rejections {
  // A member operator is implemented by an instance method, not by the static
  // Swift operator function the importer synthesizes for it.
  // expected-error@+2{{operator function '==' does not match instance method declared in header}}
  @cxx @implementation
  static func == (lhs: Rejections, rhs: Rejections) -> Bool { return true }

  // The importer's `__operatorX` is a Swift name, not the C++ name.
  // expected-error@+1{{could not find imported function '__operatorPlusEqual' matching instance method 'plusEqualsBySwiftName'; make sure you import the module or header that declares it}}
  @cxx(__operatorPlusEqual) @implementation
  mutating func plusEqualsBySwiftName(_ k: Int32) -> UnsafeMutablePointer<Rejections> { fatalError() }

  // The dropped reference result must still be produced.
  // expected-error@+2{{instance method 'plusEquals' of type '(Int32) -> ()' does not match type '(CInt) -> UnsafeMutablePointer<Rejections>' (aka '(Int32) -> UnsafeMutablePointer<Rejections>') declared by the header}}
  @cxx(`operator+=`) @implementation
  mutating func plusEquals(_ k: Int32) {}

  // An operator the importer does not import has nothing to implement.
  // expected-error@+1{{could not find imported function 'operator=' matching instance method 'assign'; make sure you import the module or header that declares it}}
  @cxx(`operator=`) @implementation
  mutating func assign(_ other: Rejections) -> UnsafeMutablePointer<Rejections> { fatalError() }
}

// Both spellings name the same C++ operator.

// expected-note@+2{{previously implemented here}}
@unsafe @cxx @implementation
func != (a: Duplicate, b: Duplicate) -> Bool { return true }

// expected-error@+1{{duplicate implementation of imported operator function '!='}}
@unsafe @cxx(`operator!=`) @implementation
func notEqual(_ a: Duplicate, _ b: Duplicate) -> Bool { return true }
