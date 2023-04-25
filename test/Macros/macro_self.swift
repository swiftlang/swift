// RUN: %target-swift-frontend -parse %s -verify

@freestanding(expression) // expected-error {{expected expression}}
macro self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro")

func sync() {}

@freestanding(expression) // expected-error {{expected expression}}
macro Self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro")

func testSelfAsFreestandingMacro() {
  _ = #self // expected-error {{expected a macro identifier for a pound literal expression}}
}

func testCapitalSelfAsFreestandingMacro() {
  _ = #Self // expected-error {{expected a macro identifier for a pound literal expression}}
}
 
func testSelfAsAttachedMacro() {
  @self // expected-error {{expected expression}}
  struct Foo {}
}

func testCapitalSelfAsAttachedMacro() {
  @Self // expected-error {{expected expression}}
  struct Foo {}
}
