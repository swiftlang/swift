// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest

@attached(accessor) macro m1() = #externalMacro(module: "MyMacros", type: "Macro1")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro1' could not be found for macro 'm1()'}}
// expected-note@-2{{'m1()' declared here}}

@attached(accessor) macro m2(_: Int) -> Void = #externalMacro(module: "MyMacros", type: "Macro2")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro2' could not be found for macro 'm2'}}
// expected-note@-2{{candidate has partially matching parameter list (Int)}}

@attached(accessor) macro m2(_: Double) -> Void = #externalMacro(module: "MyMacros", type: "Macro2")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro2' could not be found for macro 'm2'}}
// expected-note@-2{{candidate has partially matching parameter list (Double)}}

@m1 struct X1 { }

@m2 struct X2 { } // expected-error{{no exact matches in call to macro 'm2'}}

// Check for nesting rules.
struct SkipNestedType {
  @propertyWrapper
  struct m1<T> {
    init() { }

    var wrappedValue: T
  }

  // We select the macro, not the property wrapper.
  @m1 var x: Int = 0
  // expected-error@-1{{external macro implementation type 'MyMacros.Macro1' could not be found for macro 'm1()'; the type must be public and provided via '-load-plugin-library'}}
}
