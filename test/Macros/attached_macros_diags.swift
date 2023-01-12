// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest

@declaration(attached) macro m1: Void = #externalMacro(module: "MyMacros", type: "Macro1")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro1' could not be found for macro 'm1'}}

@declaration(attached) macro m2(_: Int) -> Void = #externalMacro(module: "MyMacros", type: "Macro2")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro2' could not be found for macro 'm2'}}
// expected-note@-2 2{{macro 'm2' declared here}}

@declaration(attached) macro m2(_: Double) -> Void = #externalMacro(module: "MyMacros", type: "Macro2")
// expected-warning@-1{{external macro implementation type 'MyMacros.Macro2' could not be found for macro 'm2'}}
// expected-note@-2 2{{macro 'm2' declared here}}

@m1 struct X1 { }

// FIXME: Redundant diagnostic
@m2 struct X2 { } // expected-error 2{{ambiguous reference to macro 'm2'}}

// Check for nesting rules.
struct SkipNestedType {
  @propertyWrapper
  struct m1<T> {
    init() { }

    var wrappedValue: T
  }

  // We select the macro, not the property wrapper.
  @m1 var x: Int = 0
}
