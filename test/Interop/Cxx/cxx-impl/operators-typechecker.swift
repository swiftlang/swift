// Matching rules for `@cxx @implementation` of C++ operators.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Operators


// Member operators are implemented by methods, matched by the C++ name.

extension Vector {
  @unsafe @cxx(`operator==`) @implementation
  func equals(_ other: Vector) -> Bool { return x == other.x }

  @unsafe @cxx(`operator<`) @implementation
  func less(_ other: Vector) -> Bool { return x < other.x }

  // Overloads are selected by parameter types...
  @unsafe @cxx(`operator+`) @implementation
  func plus(_ other: Vector) -> Vector { return Vector(x: x + other.x) }

  @cxx(`operator+`) @implementation
  func plus(_ k: Int32) -> Vector { return Vector(x: x + k) }

  // ...and by arity.
  @cxx(`operator-`) @implementation
  func negated() -> Vector { return Vector(x: -x) }

  @unsafe @cxx(`operator-`) @implementation
  func minus(_ other: Vector) -> Vector { return Vector(x: x - other.x) }

  // The importer drops the `Vector &` result; return it as a pointer.
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

  // Postfix has an `int` parameter.
  @cxx(`operator++`) @implementation
  mutating func postIncrement(_: Int32) -> Vector {
    let old = self
    x += 1
    return old
  }
}

// Free operators are implemented at the top level, even if namespaced.

@unsafe @cxx @implementation
func != (a: Vector, b: Vector) -> Bool { return a.x != b.x }

@unsafe @cxx(`operator*`) @implementation
func times(_ a: Vector, _ k: Int32) -> Vector { return Vector(x: a.x * k) }

@unsafe @cxx @implementation
func == (a: Outer.Point, b: Outer.Point) -> Bool { return a.v == b.v }

// Foreign reference type

@available(SwiftStdlib 5.8, *)
extension Handle {
  @cxx(`operator==`) @implementation
  func equals(_ other: Handle) -> Bool { return value == other.value }

  // Returns an unretained foreign reference.
  // expected-error@+2{{instance method 'plusEquals' cannot implement C++ function 'operator+=' because it returns a foreign reference type without a 'SWIFT_RETURNS_RETAINED' annotation, which is not yet supported}}
  @cxx(`operator+=`) @implementation
  func plusEquals(_ k: Int32) -> Handle { value += k; return self }
}

// A raw identifier like `operator==` is the C++ name under a bare `@cxx`.

extension RawIdentifier {
  @unsafe @cxx @implementation
  func `operator==`(_ other: RawIdentifier) -> Bool { return x == other.x }

  @cxx @implementation
  func `operator[]`(_ i: Int32) -> Int32 { return x + i }

  @cxx @implementation
  func `operator()`(_ i: Int32) -> Int32 { return x * i }
}

@unsafe @cxx @implementation
func `operator!=`(_ a: RawIdentifier, _ b: RawIdentifier) -> Bool { return a.x != b.x }

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
  // Must be an instance method, not the synthesized static operator.
  // expected-error@+2{{operator function '==' does not match instance method declared in header}}
  @cxx @implementation
  static func == (lhs: Rejections, rhs: Rejections) -> Bool { return true }

  // `__operatorX` is not the C++ name.
  // expected-error@+1{{could not find imported function '__operatorPlusEqual' matching instance method 'plusEqualsBySwiftName'; make sure you import the module or header that declares it}}
  @cxx(__operatorPlusEqual) @implementation
  mutating func plusEqualsBySwiftName(_ k: Int32) -> UnsafeMutablePointer<Rejections> { fatalError() }

  // Must return the dropped reference result.
  // expected-error@+2{{instance method 'plusEquals' of type '(Int32) -> ()' does not match type '(CInt) -> UnsafeMutablePointer<Rejections>' (aka '(Int32) -> UnsafeMutablePointer<Rejections>') declared by the header}}
  @cxx(`operator+=`) @implementation
  mutating func plusEquals(_ k: Int32) {}

  // `operator=` is not imported.
  // expected-error@+1{{could not find imported function 'operator=' matching instance method 'assign'; make sure you import the module or header that declares it}}
  @cxx(`operator=`) @implementation
  mutating func assign(_ other: Rejections) -> UnsafeMutablePointer<Rejections> { fatalError() }

  // Rvalue reference results are not supported, even when the importer drops
  // them.
  // expected-error@+2{{instance method 'minusEquals' cannot implement C++ function 'operator-=' because rvalue reference parameters and return types are not yet supported}}
  @cxx(`operator-=`) @implementation
  mutating func minusEquals(_ k: Int32) {}

  // expected-error@+2{{instance method 'plus' cannot implement C++ function 'operator+' because rvalue reference parameters and return types are not yet supported}}
  @cxx(`operator+`) @implementation
  func plus(_ k: Int32) -> UnsafeMutablePointer<Rejections> { fatalError() }
}

// Both spellings name the same C++ operator.

// expected-note@+2{{previously implemented here}}
@unsafe @cxx @implementation
func != (a: Duplicate, b: Duplicate) -> Bool { return true }

// expected-error@+1{{duplicate implementation of imported operator function '!='}}
@unsafe @cxx(`operator!=`) @implementation
func notEqual(_ a: Duplicate, _ b: Duplicate) -> Bool { return true }

// Not supported yet

// FIXME: The importer does not import conversion operators. It replaces
// `operator bool() const` with a synthesized `__convertToBool()` that calls it.
extension Convertible {
  // expected-error@+1{{could not find imported function 'operator bool' matching instance method 'toBool()'; make sure you import the module or header that declares it}}
  @cxx(`operator bool`) @implementation
  func toBool() -> Bool { return value != 0 }

  // expected-error@+1{{could not find imported function 'operator int' matching instance method 'toInt()'; make sure you import the module or header that declares it}}
  @cxx(`operator int`) @implementation
  func toInt() -> Int32 { return value }

  // expected-error@+1{{could not find imported function 'operator ConversionTarget' matching instance method 'toTarget()'; make sure you import the module or header that declares it}}
  @cxx(`operator ConversionTarget`) @implementation
  func toTarget() -> ConversionTarget { return ConversionTarget(v: value) }

  // The synthesized `__convertToBool()` is not the C++ operator.
  // expected-error@+2{{instance method 'convertToBool()' cannot implement C++ function '__convertToBool' because it already has a definition}}
  @cxx(__convertToBool) @implementation
  func convertToBool() -> Bool { return value != 0 }
}
