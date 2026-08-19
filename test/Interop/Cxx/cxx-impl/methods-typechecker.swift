// Matching rules for `@cxx @implementation` methods declared in Swift
// extensions of imported C++ structs and classes.

// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -disable-objc-interop \
// RUN:   -I %S/Inputs

// REQUIRES: swift_feature_CxxImplementation

import Methods


// Static, const, and non-const methods, arity overloads, and a rename onto a
// method all match with no diagnostics. A const method is implemented by a
// non-mutating method and a non-const method by a `mutating` one.

extension Counter {
  @cxx @implementation
  static func make(_ v: Int32) -> Counter { return Counter(value: v) }

  @cxx @implementation
  func get() -> Int32 { return value }

  @cxx @implementation
  mutating func add(_ d: Int32) { value += d }

  @cxx @implementation
  func overloadedByArity() -> Int32 { return value }

  @cxx @implementation
  func overloadedByArity(_ x: Int32) -> Int32 { return value + x }

  @cxx(renamedTarget) @implementation
  func swiftRenamed() -> Int32 { return value }
}


// A const and a non-const overload are told apart by `mutating`. The importer
// names the non-const overloads `adjustMutating`; an implementation is matched
// by its C++ name, so it may be declared under either name.

extension Pair {
  // expected-note@+2{{previously implemented here}}
  @cxx @implementation
  func adjust(_ x: Int32) -> Int32 { return value + x }

  @cxx(adjust) @implementation
  mutating func adjustMutating(_ x: Int32) -> Int32 { value += x; return value }

  @cxx @implementation
  mutating func adjust(_ x: Int32, _ y: Int32) -> Int32 { value += x + y; return value }
}

// A second implementation of the const overload is a duplicate; the
// implementation of the non-const overload above is not.

extension Pair {
  // expected-error@+1{{duplicate implementation of imported instance method 'adjust'}}
  @cxx(adjust) @implementation
  func adjustAlias(_ x: Int32) -> Int32 { return value + x }
}


// The receiver may be a non-trivial C++ class: `this` is a pointer, so the
// receiver is not copied at the boundary.

extension NonTrivialReceiver {
  @cxx @implementation
  func read() -> Int32 { return value }

  @cxx @implementation
  mutating func write(_ v: Int32) { value = v }
}


// A non-virtual method of a polymorphic class is fine.

extension Polymorphic {
  @cxx @implementation
  func nonVirtualMethod() -> Int32 { return 0 }
}


// Name not declared in the struct

extension Counter {
  // expected-error@+1{{could not find imported function 'notDeclared' matching instance method 'notDeclared()'; make sure you import the module or header that declares it}}
  @cxx @implementation
  func notDeclared() -> Int32 { return value }
}


// `mutating` must match the method's constness

extension Rejections {
  // expected-error@+2{{instance method 'constMethod()' should not be 'mutating' to match 'const' instance method declared by the header}}
  @cxx @implementation
  mutating func constMethod() -> Int32 { return value }

  // expected-error@+2{{instance method 'nonConstMethod()' should be 'mutating' to match non-'const' instance method declared by the header}}
  @cxx @implementation
  func nonConstMethod() {}
}


// Static and instance methods do not match each other

extension Rejections {
  // expected-error@+2{{static method 'instanceMethod()' does not match instance method declared in header}}
  @cxx @implementation
  static func instanceMethod() -> Int32 { return 0 }

  // expected-error@+2{{instance method 'staticMethod()' does not match static method declared in header}}
  @cxx @implementation
  func staticMethod() -> Int32 { return value }
}


// Inline

extension Rejections {
  // expected-error@+2{{instance method 'inlineMethod()' cannot implement C++ function 'inlineMethod' because it already has a definition}}
  @cxx @implementation
  func inlineMethod() -> Int32 { return value }
}


// Virtual

extension Polymorphic {
  // expected-error@+2{{instance method 'virtualMethod()' cannot implement C++ function 'virtualMethod' because virtual methods are not yet supported}}
  @cxx @implementation
  func virtualMethod() -> Int32 { return 0 }
}


// A method inherited from a base class is not a method of the derived class.

extension Derived {
  // expected-error@+1{{could not find imported function 'baseMethod' matching instance method 'baseMethod()'; make sure you import the module or header that declares it}}
  @cxx @implementation
  func baseMethod() -> Int32 { return 0 }

  @cxx @implementation
  func derivedMethod() -> Int32 { return 0 }
}


// Methods of a foreign reference type match like those of a value type.

@available(SwiftStdlib 5.8, *)
extension Widget {
  @cxx @implementation
  func tag() -> Int32 { return id }

  // expected-error@+2{{instance method 'describe()' cannot implement C++ function 'describe' because virtual methods are not yet supported}}
  @cxx @implementation
  func describe() -> Int32 { return id }

  @cxx @implementation
  static func count() -> Int32 { return 0 }
}
