// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -module-name MacrosTest

@expression macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
// expected-note@-1 2{{'stringify' declared here}}
@expression macro missingMacro1(_: Any) = MissingModule.MissingType // expected-note{{'missingMacro1' declared here}}
// expected-warning@-1{{external macro definitions are now written using #externalMacro}}{{43-68=#externalMacro(module: "MissingModule", type: "MissingType")}}
@expression macro missingMacro2(_: Any) = MissingModule.MissingType

protocol P { }

@expression macro tryToHide<T: P>(_: P) -> some P = BuiltinMacros.Blah
// expected-error@-1{{some' types are only permitted in properties, subscripts, and functions}}

internal struct X { } // expected-note{{type declared here}}

@expression public macro createAnX: X = BuiltinMacros.Blah
// expected-error@-1{{macro cannot be declared public because its result type uses an internal type}}

@expression macro m1: Int = A.B
@expression macro m1: Float = A.B

@expression macro m2: Int = A.B // expected-note{{'m2' previously declared here}}
@expression macro m2: Int = A.B // expected-error{{invalid redeclaration of 'm2'}}

@expression macro m3(_: Int) -> Int = A.B
@expression macro m3(_: Int) -> Float = A.B

@expression macro m4(_: Int) -> Int = A.B // expected-note{{'m4' previously declared here}}
@expression macro m4(_: Int) -> Int = A.B // expected-error{{invalid redeclaration of 'm4'}}

struct ZZZ {
  macro m5: Int = A.B
  // expected-error@-1{{macro 'm5' can only be declared at file scope}}
  // expected-error@-2{{macro 'm5' must declare its applicable contexts (e.g., '@expression')}}
}

@expression macro multiArgMacro(_: Any, second: Any) = MissingModule.MissingType
// expected-note@-1{{'multiArgMacro(_:second:)' declared here}}

@expression macro overloaded1(_ p: P) = MissingModule.MissingType
func overloaded1(_ p: Any) { }

@expression macro notOverloaded1(_ p: P) = MissingModule.MissingType // expected-note{{'notOverloaded1' previously declared here}}
@expression macro notOverloaded1(_ p: P) = MissingModule.MissingOtherType // expected-error{{invalid redeclaration of 'notOverloaded1'}}

@expression macro intIdentity(value: Int, _: Float) -> Int = MissingModule.MissingType
// expected-note@-1{{macro 'intIdentity(value:_:)' declared here}}

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
}

func shadow(a: Int, b: Int, stringify: Int) {
  _ = #stringify(a + b)
  // expected-error@-1{{external macro implementation type 'MacroDefinition.StringifyMacro' could not be found for macro 'stringify'}}
}

func testMissing() {
  #missingMacro1("hello") // expected-error{{external macro implementation type 'MissingModule.MissingType' could not be found for macro 'missingMacro1'; the type must be public and provided via '-load-plugin-library'}}
}
