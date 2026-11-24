// Matching rules for `@cxx @implementation` of C++23 operators.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -Xcc -std=c++23 \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import OperatorsCxx23


// `operator[]` with any number of parameters is implemented like any other
// member operator.

extension Grid {
  // Overloads are selected by arity...
  @cxx(`operator[]`) @implementation
  func first() -> Int32 { return width }

  @cxx(`operator[]`) @implementation
  func at(_ i: Int32) -> Int32 { return i }

  @cxx(`operator[]`) @implementation
  func at(_ row: Int32, _ col: Int32) -> Int32 { return row * width + col }

  // ...and by parameter types.
  @cxx(`operator[]`) @implementation
  func at(_ row: Double, _ col: Double) -> Double {
    return row * Double(width) + col
  }

  // A reference result is returned as a pointer.
  @cxx(`operator[]`) @implementation
  mutating func at(_ row: Int32, _ col: Int32, _ layer: Int32) -> UnsafeMutablePointer<Int32> {
    return withUnsafeMutablePointer(to: &width) { $0 }
  }

  // Rejections

  // No overload takes four indices.
  // expected-error@+1{{could not find imported function 'operator[]' matching instance method 'at'; make sure you import the module or header that declares it}}
  @cxx(`operator[]`) @implementation
  func at(_ a: Int32, _ b: Int32, _ c: Int32, _ d: Int32) -> Int32 { return 0 }

  // No overload takes these index types.
  // expected-error@+1{{could not find imported function 'operator[]' matching instance method 'at'; make sure you import the module or header that declares it}}
  @cxx(`operator[]`) @implementation
  func at(_ row: Int32, _ col: Double) -> Int32 { return 0 }
}

// A static `operator[]` is implemented by a static method.

extension StaticGrid {
  @cxx(`operator[]`) @implementation
  static func at(_ i: Int32) -> Int32 { return i * 2 }

  @cxx(`operator[]`) @implementation
  static func at(_ row: Int32, _ col: Int32) -> Int32 { return row * 10 + col }

  // Must be a static method.
  // expected-error@+2{{instance method 'instanceAt' does not match static method declared in header}}
  @cxx(`operator[]`) @implementation
  func instanceAt(_ i: Int32) -> Int32 { return 0 }
}

// Not supported yet

// FIXME: The importer replaces a static `operator()` with a synthesized
// instance method that forwards to it, so nothing imported has the C++ name.
extension StaticCall {
  // expected-error@+1{{could not find imported function 'operator()' matching static method 'call'; make sure you import the module or header that declares it}}
  @cxx(`operator()`) @implementation
  static func call(_ x: Int32) -> Int32 { return x }
}

// FIXME: The importer does not import explicit object member functions.
extension DeducingThis {
  // expected-error@+1{{could not find imported function 'operator==' matching instance method 'equals'; make sure you import the module or header that declares it}}
  @unsafe @cxx(`operator==`) @implementation
  func equals(_ other: DeducingThis) -> Bool { return value == other.value }
}
