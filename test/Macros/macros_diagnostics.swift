// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest

@expression macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
// expected-note@-1 2{{'stringify' declared here}}
// expected-warning@-2{{external macro implementation type}}
// expected-warning@-3{{@expression has been removed in favor of @freestanding(expression)}}{{1-12=@freestanding(expression)}}
@freestanding(expression) macro missingMacro1(_: Any) = MissingModule.MissingType // expected-note{{'missingMacro1' declared here}}
// expected-warning@-1{{external macro definitions are now written using #externalMacro}}{{57-82=#externalMacro(module: "MissingModule", type: "MissingType")}}
// expected-warning@-2{{external macro implementation type}}
@freestanding(expression) macro missingMacro2(_: Any) = #externalMacro(module: "MissingModule", type: "MissingType")
// expected-warning@-1{{external macro implementation type}}

protocol P { }

@freestanding(expression) macro tryToHide<T: P>(_: T) -> some P = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-error@-1{{some' types are only permitted in properties, subscripts, and functions}}
// expected-error@-2{{generic parameter 'T' could not be inferred}}

internal struct X { } // expected-note{{type declared here}}

@freestanding(expression) public macro createAnX: X = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-error@-1{{macro cannot be declared public because its result type uses an internal type}}
// expected-warning@-2{{external macro implementation type}}

@freestanding(expression) macro m1: Int = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-warning@-1{{external macro implementation type}}
@freestanding(expression) macro m1: Float = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-warning@-1{{external macro implementation type}}

@freestanding(expression) macro m2: Int = #externalMacro(module: "BuiltinMacros", type: "Blah") // expected-note{{'m2' previously declared here}}
// expected-warning@-1{{external macro implementation type}}
@freestanding(expression) macro m2: Int = #externalMacro(module: "BuiltinMacros", type: "Blah") // expected-error{{invalid redeclaration of 'm2'}}
// expected-warning@-1{{external macro implementation type}}

@freestanding(expression) macro m3(_: Int) -> Int = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-warning@-1{{external macro implementation type}}
@freestanding(expression) macro m3(_: Int) -> Float = #externalMacro(module: "BuiltinMacros", type: "Blah")
// expected-warning@-1{{external macro implementation type}}

@freestanding(expression) macro m4(_: Int) -> Int = #externalMacro(module: "BuiltinMacros", type: "Blah") // expected-note{{'m4' previously declared here}}
// expected-warning@-1{{external macro implementation type}}
@freestanding(expression) macro m4(_: Int) -> Int = #externalMacro(module: "BuiltinMacros", type: "Blah") // expected-error{{invalid redeclaration of 'm4'}}
// expected-warning@-1{{external macro implementation type}}

struct ZZZ {
  macro m5: Int = #externalMacro(module: "BuiltinMacros", type: "Blah")
  // expected-error@-1{{macro 'm5' can only be declared at file scope}}
  // expected-error@-2{{macro 'm5' must declare its applicable roles}}
  // expected-warning@-3{{external macro implementation type}}
}

@freestanding(expression) macro multiArgMacro(_: Any, second: Any) = #externalMacro(module: "MissingModule", type: "MissingType")
// expected-note@-1{{'multiArgMacro(_:second:)' declared here}}
// expected-warning@-2{{external macro implementation type}}

@freestanding(expression) macro overloaded1(_ p: P) = #externalMacro(module: "MissingModule", type: "MissingType")
// expected-warning@-1{{external macro implementation type}}

func overloaded1(_ p: Any) { }

@freestanding(expression) macro notOverloaded1(_ p: P) = #externalMacro(module: "MissingModule", type: "MissingType") // expected-note{{'notOverloaded1' previously declared here}}
// expected-warning@-1{{external macro implementation type}}
@freestanding(expression) macro notOverloaded1(_ p: P) = #externalMacro(module: "MissingModule", type: "MissingOtherType") // expected-error{{invalid redeclaration of 'notOverloaded1'}}
// expected-warning@-1{{external macro implementation type}}

@freestanding(expression) macro intIdentity(value: Int, _: Float) -> Int = #externalMacro(module: "MissingModule", type: "MissingType")
// expected-note@-1{{macro 'intIdentity(value:_:)' declared here}}
// expected-warning@-2{{external macro implementation type}}

@declaration(freestanding) macro unaryDeclMacro(_ x: String) // expected-note {{found this candidate}}
// expected-error @-1 {{macro 'unaryDeclMacro' requires a definition}}
@declaration(freestanding) macro unaryDeclMacro(_ x: String, blah: Bool) // expected-note {{found this candidate}}
// expected-error @-1 {{macro 'unaryDeclMacro(_:blah:)' requires a definition}}

func testDiags(a: Int, b: Int) {
  // FIXME: Bad diagnostic.
  let s = #stringify<Int, Int>(a + b) // expected-error{{type of expression is ambiguous without more context}}

  _ = #stringify()
  // expected-error@-1{{missing argument for parameter #1 in macro expansion}}
  _ = #stringify(label: a + b)
  // expected-error@-1{{extraneous argument label 'label:' in macro expansion}}

  _ = #multiArgMacro() // expected-error{{missing arguments for parameters #1, 'second' in macro expansion}}
  _ = #multiArgMacro(1, 2) // expected-error{{missing argument label 'second:' in macro expansion}}{{25-25=second: }}

  _ = #multiArgMacro(1, second: 2) { } // expected-error{{extra trailing closure passed in macro expansion}}
  _ = #multiArgMacro(1, second: 2, 3) // expected-error{{extra argument in macro expansion}}
  _ = #multiArgMacro(1, second: 2, third: 3) // expected-error{{extra argument 'third' in macro expansion}}

  _ = stringify(a + b)
  // expected-error@-1{{expansion of macro 'stringify' requires leading '#'}}{{7-7=#}}

  _ = #intIdentity // expected-error{{expansion of macro 'intIdentity(value:_:)' requires arguments}}{{19-19=(value: <#Int#>, <#Float#>)}}

  overloaded1(a) // okay, calls the function
  #overloaded1(a) // expected-error{{argument type 'Int' does not conform to expected type 'P'}}

//  #unaryDeclMacro("abc") // okay, declaration macro where both exprs and decls are allowed
//  _ = #unaryDeclMacro("abc") // xpected-error {{no macro named 'unaryDeclMacro'}}
//  (#unaryDeclMacro("abc"), 3) // xpected-error {{no macro named 'unaryDeclMacro'}}

  struct Foo {
    #unaryDeclMacro("abc", blah: false) // okay
    #unaryDeclMacro("abc", blah: false, oh: 2) // expected-error {{no exact matches in reference to macro 'unaryDeclMacro'}}
  }
}

func shadow(a: Int, b: Int, stringify: Int) {
  _ = #stringify(a + b)
  // expected-error@-1{{external macro implementation type 'MacroDefinition.StringifyMacro' could not be found for macro 'stringify'}}
}

func testMissing() {
  #missingMacro1("hello") // expected-error{{external macro implementation type 'MissingModule.MissingType' could not be found for macro 'missingMacro1'; the type must be public and provided via '-load-plugin-library'}}
}

@freestanding(expression) macro undefined() // expected-error{{macro 'undefined()' requires a definition}}

func testExternalMacroOutOfPlace() {
  let _: Int = #externalMacro(module: "A", type: "B")
  // expected-error@-1{{macro 'externalMacro' can only be used to define another macro}}
}
