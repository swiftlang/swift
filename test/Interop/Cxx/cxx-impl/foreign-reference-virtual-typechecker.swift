// `@cxx @implementation` of overrides in foreign reference types. The Swift
// function is the body of the C++ override, not a Swift override.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -target %target-swift-5.8-abi-triple \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}foreign-reference-virtual.h \
// RUN:   -verify-ignore-unrelated \
// RUN:   -I %S%{fs-sep}Inputs

// REQUIRES: swift_feature_CxxImplementation

import ForeignReferenceVirtual


extension Base {
  @cxx @implementation
  public func anchor() -> Int32 { return 0 }

  @cxx @implementation
  public func describe() -> Int32 { return value }
}


// An override, and a method hiding the base's.

extension Derived {
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() * 2 }

  @cxx @implementation
  public func hide() -> Int32 { return super.hide() + 1 }
}


// An override of a method Derived only inherits.

extension Leaf {
  @cxx @implementation
  public func tag() -> Int32 { return super.tag() + 1 }
}


// Overrides of the primary and the non-primary base's methods.

extension MultiDerived {
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() * 3 }

  @cxx @implementation
  public func fromSecond() -> Int32 { return value + second }
}


// An override of a pure virtual method.

extension ConcreteDerived {
  @cxx @implementation
  public func pure() -> Int32 {
    // expected-error@+1{{cannot use 'super' to call C++ pure virtual method 'pure()'; it has no base class implementation}}
    return super.pure()
  }
}


// `override` is rejected: C++ declares what the method overrides.

extension Leaf {
  // expected-error@+2{{'override' cannot be combined with '@cxx'; whether 'describe()' overrides a base class method is determined by its C++ declaration}}{{10-19=}}
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() + 1 }
}

extension ValueDerived {
  // expected-error@+2{{'override' cannot be combined with '@cxx'; whether 'get()' overrides a base class method is determined by its C++ declaration}}{{10-19=}}
  @cxx @implementation
  public override func get() -> Int32 { return 1 }
}


// Derived inherits tag() without declaring it.

extension Derived {
  // expected-error@+1{{could not find imported function 'tag' matching instance method 'tag()'; make sure you import the module or header that declares it}}
  @cxx @implementation
  public func tag() -> Int32 { return 0 }
}


// Other extension methods still cannot override, with or without `override`.

extension MultiDerived {
  // expected-error@+2{{overriding non-open instance method outside of its defining module}}
  // expected-error@+1{{instance method 'tag()' declared in 'Base' cannot be overridden from extension}}
  public override func tag() -> Int32 { return 0 }

  // expected-error@+2{{overriding non-open instance method outside of its defining module}}
  // expected-error@+1{{instance method 'anchor()' declared in 'Base' cannot be overridden from extension}}
  public func anchor() -> Int32 { return 0 }
}
