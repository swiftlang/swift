// `@cxx @implementation` of overrides in foreign reference types. The Swift
// function is the body of the C++ override, not a Swift override. It is marked
// `override` when the C++ method overrides a method of one of its Swift
// superclasses.

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


// An override.

extension Derived {
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() * 2 }
}


// An override of a method Derived only inherits.

extension Leaf {
  @cxx @implementation
  public override func tag() -> Int32 { return super.tag() + 1 }
}


// An override of the primary base's method.

extension MultiDerived {
  @cxx @implementation
  public override func describe() -> Int32 { return super.describe() * 3 }
}


// An override of a pure virtual method.

extension ConcreteDerived {
  @cxx @implementation
  public override func pure() -> Int32 {
    // expected-error@+1{{cannot use 'super' to call C++ pure virtual method 'pure()'; it has no base class implementation}}
    return super.pure()
  }
}


// `override` is required on an override of a superclass's method.

extension Leaf {
  // expected-error@+2{{overriding declaration requires an 'override' keyword}}{{10-10=override }}
  @cxx @implementation
  public func describe() -> Int32 { return super.describe() + 1 }
}


// `override` is rejected on any other method.

extension Derived {
  // expected-error@+2{{instance method 'hide()' cannot be marked 'override' because its C++ declaration does not override a base class method}}{{10-19=}}
  @cxx @implementation
  public override func hide() -> Int32 { return super.hide() + 1 }
}

extension MultiDerived {
  // expected-error@+2{{instance method 'fromSecond()' cannot be marked 'override' because the C++ method it overrides belongs to 'SecondBase', which is not a superclass of 'MultiDerived' in Swift}}{{10-19=}}
  @cxx @implementation
  public override func fromSecond() -> Int32 { return value + second }
}

extension ValueDerived {
  // expected-error@+2{{instance method 'get()' cannot be marked 'override' because the C++ method it overrides belongs to 'ValueBase', which is not a superclass of 'ValueDerived' in Swift}}{{10-19=}}
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
