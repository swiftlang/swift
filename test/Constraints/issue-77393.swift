// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/77393
// Make sure we don't crash.

@freestanding(expression)
macro someMacro() = #externalMacro(module: "", type: "")
// expected-warning@-1 {{external macro implementation type '.' could not be found for macro 'someMacro()'; plugin for module '' not found}}
// expected-note@-2 {{'someMacro()' previously declared here}}
// expected-note@-3 {{'someMacro()' declared here}}

@freestanding(expression)
macro someMacro() = #externalMacro(module: "", type: "")
// expected-error@-1 {{invalid redeclaration of 'someMacro()'}}
// expected-warning@-2 {{external macro implementation type '.' could not be found for macro 'someMacro()'; plugin for module '' not found}}

#someMacro()
// expected-error@-1 {{external macro implementation type '.' could not be found for macro 'someMacro()'; plugin for module '' not found}}

macro invalidMacro()
// expected-error@-1 {{macro 'invalidMacro()' requires a definition}}
// expected-error@-2 {{macro 'invalidMacro()' must declare its applicable roles via '@freestanding' or '@attached'}}

#invalidMacro() // expected-error {{no macro named 'invalidMacro'}}
