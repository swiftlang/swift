// RUN: %target-swift-frontend -parse %s -verify

@freestanding(expression)
macro self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro")
// expected-error@-1 {{expected declaration}}

func sync() {}

@freestanding(expression)
macro Self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro")
// expected-error@-1 {{expected declaration}}

func testSelfAsFreestandingMacro() {
  _ = #self
}

func testCapitalSelfAsFreestandingMacro() {
  _ = #Self
}
 
func testSelfAsAttachedMacro() {
  @self // expected-error {{expected expression}}
  struct Foo {}
}

func testCapitalSelfAsAttachedMacro() {
  @Self // expected-error {{expected expression}}
  struct Foo {}
}
