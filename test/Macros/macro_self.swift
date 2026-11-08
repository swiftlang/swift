// RUN: %target-swift-frontend -parse %s -verify

@freestanding(expression)
macro self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro") // expected-astgen-note {{if this name is unavoidable, use backticks to escape it}}
// expected-cparser-error@-1 {{expected declaration}} expected-astgen-error@-1 {{keyword 'self' cannot be used as an identifier here}}

func sync() {}

@freestanding(expression)
macro Self() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro") // expected-astgen-note {{if this name is unavoidable, use backticks to escape it}}
// expected-cparser-error@-1 {{expected declaration}} expected-astgen-error@-1 {{keyword 'Self' cannot be used as an identifier here}}

func testSelfAsFreestandingMacro() {
  _ = #self
}

func testCapitalSelfAsFreestandingMacro() {
  _ = #Self
}
 
func testSelfAsAttachedMacro() {
  @self // expected-cparser-error {{expected expression}} expected-astgen-error {{unexpected code '@self' in function}}
  struct Foo {}
}

func testCapitalSelfAsAttachedMacro() {
  @Self // expected-cparser-error {{expected expression}}
  struct Foo {}
}
