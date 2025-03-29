// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -parse-as-library -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -disable-availability-checking -module-name MacrosTest

@attached(peer) macro m1() = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")

@attached(peer) macro m2(_: Int) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
// expected-note@-1{{'m2' declared here}}
// expected-note@-2{{candidate expects value of type 'Int' for parameter #1 (got 'String')}}

@attached(peer) macro m2(_: Double) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
// expected-note@-1{{candidate expects value of type 'Double' for parameter #1 (got 'String')}}

@attached(peer) macro m3(message: String) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
// expected-note@-1{{'m3(message:)' declared here}}

@attached(peer) macro m4(_ param1: Int, label2 param2: String) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
// expected-note@-1 3 {{'m4(_:label2:)' declared here}}

@attached(peer) macro m5(label1: Int, label2: String) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")
// expected-note@-1{{'m5(label1:label2:)' declared here}}

@attached(peer) macro m6(label: Int = 42) = #externalMacro(module: "MacroDefinition", type: "EmptyPeerMacro")

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MyMacros", type: "StringifyMacro")
// expected-warning@-1{{external macro implementation type 'MyMacros.StringifyMacro' could not be found for macro 'stringify'}}
// expected-note@-2{{'stringify' declared here}}

@m1 struct X1 { }

@m2 struct X2 { } // expected-error{{missing argument for parameter #1 in macro expansion}}{{4-4=(<#Int#>)}}

@m3 struct X3 { } // expected-error{{missing argument for parameter 'message' in macro expansion}}{{4-4=(message: <#String#>)}}

@m4 struct X4 { } // expected-error{{missing arguments for parameters #1, 'label2' in macro expansion}}{{4-4=(<#Int#>, label2: <#String#>)}}

@m5 struct X5 { } // expected-error{{missing arguments for parameters 'label1', 'label2' in macro expansion}}{{4-4=(label1: <#Int#>, label2: <#String#>)}}

@m6 struct X6 { }

// Check for nesting rules.
struct SkipNestedType {
  @propertyWrapper
  struct m1<T> {
    init() { }

    var wrappedValue: T
  }

  // We select the macro, not the property wrapper.
  @m1 var x: Int = 0
  //expected-note@-1{{did you mean 'x'?}}

  func test() {
    let _: m1<Int> = _x
    // expected-error@-1{{cannot find '_x' in scope}}
  }
}

struct TestMacroArgs {
  @m1("extra arg") struct Args1 {} // expected-error{{argument passed to macro expansion that takes no arguments}}

  @m2(10) struct Args2 {}

  @m2(10.0) struct Args3 {}

  @m2("") struct Args4 {} // expected-error{{no exact matches in call to macro 'm2'}}

  @m2(Nested.x) struct Args5 {}

  @m4(10) struct Args6 { } // expected-error{{missing argument for parameter 'label2' in macro expansion}}{{9-9=, label2: <#String#>}}

  @m4(label2: "test") struct Args7 { } // expected-error{{missing argument for parameter #1 in macro expansion}}{{7-7=<#Int#>, }}

  struct Nested {
    static let x = 10

    @m2(x) struct Args1 {}

    @m2(Nested.x) struct Args2 {}
  }

  @m3(message: stringify(Nested.x).1) struct Args8 {}
  // expected-error@-1{{expansion of macro 'stringify' requires leading '#'}}

  @m3(message: #stringify().1) struct Args9 {}
  // expected-error@-1{{missing argument for parameter #1 in macro expansion}}

  @m3(message: #stringify(Nested.x).1) struct Args10 {}

  // Allow macros to have arbitrary generic specialization lists, but warn
  // https://github.com/swiftlang/swift/issues/75500
  @m1<UInt> struct Args11 {} // expected-warning {{cannot specialize a non-generic external macro 'm1()'}}
}
