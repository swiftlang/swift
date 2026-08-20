// C++ virtual methods implemented in Swift via `@cxx @implementation`.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-availability-checking \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}virtual.h \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import Virtual


// Non-key, non-overriding virtual methods of a record are accepted: a const
// one is implemented by a non-mutating method, a non-const one by a `mutating`
// method, like their non-virtual siblings.

extension Shape {
  @cxx @implementation
  public func area() -> Int32 { return sides * sides }

  @cxx @implementation
  public mutating func scale(_ factor: Int32) { sides *= factor }

  // The key function itself must stay in C++: the translation unit that
  // defines it emits the class's vtable.
  // expected-error@+2{{instance method 'keyFunction()' cannot implement C++ function 'keyFunction' because it is the key function of C++ class 'Shape'}}
  @cxx @implementation
  public func keyFunction() -> Int32 { return 0 }
}


// An override along single, non-virtual inheritance with an unchanged return
// type cannot need thunks and is accepted, as is the base class's method it
// overrides.

extension SimpleBase {
  @cxx @implementation
  public func simple() -> Int32 { return stored }
}

extension SimpleDerived {
  @cxx @implementation
  public func simple() -> Int32 { return stored * 2 }
}


// A pure virtual method is rejected.

// expected-warning@+1{{'Abstract' is deprecated: abstract C++ classes cannot be used as values in Swift}}
extension Abstract {
  // expected-error@+2{{instance method 'pureMethod()' cannot implement pure virtual C++ method 'pureMethod'}}
  @cxx @implementation
  public func pureMethod() -> Int32 { return 0 }
}


// A covariant return type crossing to a base at a nonzero offset needs a
// return-adjusting thunk.

extension CloneDerived {
  // expected-error@+2{{instance method 'clone()' cannot implement C++ function 'clone' because it overrides a virtual method and changes its return type}}
  @cxx @implementation
  public mutating func clone() -> UnsafeMutablePointer<RetC> {
    return UnsafeMutablePointer(bitPattern: 1)!
  }
}


// An override of a method of a non-primary base needs a this-adjusting
// thunk; under multiple inheritance even an override of the primary base's
// method is conservatively rejected.

extension MIDerived {
  // expected-error@+2{{instance method 'firstA()' cannot implement C++ function 'firstA' because it overrides a virtual method in a class hierarchy that uses multiple inheritance}}
  @cxx @implementation
  public func firstA() {}

  // expected-error@+2{{instance method 'fromB()' cannot implement C++ function 'fromB' because it overrides a virtual method in a class hierarchy that uses multiple inheritance}}
  @cxx @implementation
  public func fromB() -> Int32 { return 0 }
}


// An override of a method of a virtual base needs a this-adjusting thunk with
// a virtual adjustment.

extension VDerived {
  // expected-error@+2{{instance method 'vbMethod()' cannot implement C++ function 'vbMethod' because it overrides a virtual method in a class hierarchy that has a virtual base}}
  @cxx @implementation
  public func vbMethod() -> Int32 { return 0 }
}


// A virtual method of a foreign reference type resolves through the importer's
// synthesized dispatch thunk to the underlying virtual method; the same rules
// apply to it.

extension Engine {
  @cxx @implementation
  public func status() -> Int32 { return rpm }

  @cxx @implementation
  public func boost(_ amount: Int32) { rpm += amount }

  // expected-error@+2{{instance method 'keyAnchor()' cannot implement C++ function 'keyAnchor' because it is the key function of C++ class 'Engine'}}
  @cxx @implementation
  public func keyAnchor() {}
}


// A pure virtual method of a foreign reference type imports (as a dispatch
// thunk), so it reaches the virtual-specific checks and is rejected there.

extension AbstractEngine {
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
