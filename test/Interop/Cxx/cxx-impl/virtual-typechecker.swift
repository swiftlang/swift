// C++ virtual methods implemented in Swift via `@cxx @implementation`: all are
// accepted except pure virtual methods.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}virtual.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import Virtual


// A const virtual method is implemented by a non-mutating method, a non-const
// one by a `mutating` method.

extension Shape {
  @cxx @implementation
  public func area() -> Int32 { return sides * sides }

  @cxx @implementation
  public mutating func scale(_ factor: Int32) { sides *= factor }
}


// An override under single inheritance, and the base method.

extension SimpleBase {
  @cxx @implementation
  public func simple() -> Int32 { return stored }
}

extension SimpleDerived {
  @cxx @implementation
  public func simple() -> Int32 { return stored * 2 }
}


// A pure virtual method is rejected; the key function of its class is not.

// expected-warning@+1{{'Abstract' is deprecated: abstract C++ classes cannot be used as values in Swift}}
extension Abstract {
  @cxx @implementation
  public func anchor() -> Int32 { return 7 }

  // expected-error@+2{{instance method 'pureMethod()' cannot implement pure virtual C++ method 'pureMethod'}}
  @cxx @implementation
  public func pureMethod() -> Int32 { return 0 }
}


// An override with a covariant return type.

extension CloneDerived {
  @cxx @implementation
  public mutating func clone() -> UnsafeMutablePointer<RetC> {
    return sharedRetC()
  }
}


// Overrides of both bases' methods under multiple inheritance.

extension MIDerived {
  @cxx @implementation
  public mutating func miAnchor() {}

  @cxx @implementation
  public mutating func firstA() { a += 100 }

  @cxx @implementation
  public func fromB() -> Int32 { return a + b }
}


// An override of a virtual base's method.

extension VDerived {
  @cxx @implementation
  public mutating func vAnchor() {}

  @cxx @implementation
  public func vbMethod() -> Int32 { return vd }
}


// A virtual method of a foreign reference type resolves through the importer's
// synthesized dispatch thunk to the underlying virtual method; the same rules
// apply to it.

extension Engine {
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }
}


// A pure virtual method of a foreign reference type is rejected too.

extension AbstractEngine {
  @cxx @implementation
  public func aeAnchor() -> Int32 { return 11 }

  // expected-error@+2{{instance method 'pureStatus()' cannot implement pure virtual C++ method 'pureStatus'}}
  @cxx @implementation
  public func pureStatus() -> Int32 { return 0 }
}


// Overloaded virtual methods are selected by parameter type: Swift implements
// the `int` overloads and leaves the `double` ones to C++.

extension Mixer {
  @cxx @implementation
  public func mix(_ amount: Int32) -> Int32 { return level + amount }
}

extension Gauge {
  @cxx @implementation
  public func read(_ scale: Int32) -> Int32 { return level * scale }
}
