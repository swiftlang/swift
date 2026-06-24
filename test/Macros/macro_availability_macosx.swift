// REQUIRES: swift_swift_parser, executable_test, OS=macosx

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// RUN: %target-typecheck-verify-swift -swift-version 5 -module-name MacrosTest -target %target-cpu-apple-macosx11 -load-plugin-library %t/%target-library-name(MacroDefinition)

@available(macOS 12.0, *)
struct X { }

@freestanding(expression) macro m1() -> X = #externalMacro(module: "A", type: "B") // expected-error{{'X' is only available in macOS 12.0 or newer}}
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm1()'}}
// expected-note@-2{{add '@available' attribute to enclosing macro}}

@available(macOS 12.0, *)
@freestanding(expression) macro m2() -> X = #externalMacro(module: "A", type: "B")
// expected-warning@-1{{external macro implementation type 'A.B' could not be found for macro 'm2()'}}

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@attached(member, names: named(synthesizedMember))
macro MemberThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "MemberThatCallsCodeMacro")

@attached(peer, names: arbitrary)
macro PeerMethodThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "PeerMethodThatCallsCodeMacro")

@attached(accessor)
macro GetterThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "GetterThatCallsCodeMacro")

@attached(body)
macro BodyThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "BodyThatCallsCodeMacro")

@attached(extension, names: arbitrary)
macro ExtensionMethodThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "ExtensionMethodThatCallsCodeMacro")

@freestanding(declaration, names: named(synthesizedDecl))
macro FuncThatCallsCode(_ codeString: String) = #externalMacro(module: "MacroDefinition", type: "FuncThatCallsCodeMacro")

@available(macOS 12.0, *)
func onlyInMacOS12() { }

func test() {
  // expected-note@-1 2{{add '@available' attribute to enclosing global function}}
  _ = #stringify({
      // expected-note@-1 {{in expansion of macro 'stringify' here}}
      if #available(macOS 12.0, *) {
        onlyInMacOS12()
      }
      onlyInMacOS12() // expected-error {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}} expected-note {{add 'if #available' version check}}
    })
  /*
  expected-expansion@-8:7{{
    expected-error@6:7 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
  }}
  */
}

@MemberThatCallsCode("""
  if #available(macOS 12.0, *) {
    onlyInMacOS12()
  }
  onlyInMacOS12()
  """)
// expected-note@-6 3{{in expansion of macro 'MemberThatCallsCode' on struct 'HasMembers' here}}
struct HasMembers {
  // expected-note@-1 {{add '@available' attribute to enclosing struct}}
  /*
  expected-expansion@+6:1{{
    expected-note@1:12 {{add '@available' attribute to enclosing static property}}
    expected-error@5:3 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
    expected-note@5:3 {{add 'if #available' version check}}
  }}
  */
}

// When the attached type has @available, the member expansion correctly
// inherits the type's availability and the call is allowed.
@available(macOS 12.0, *)
@MemberThatCallsCode("onlyInMacOS12()")
struct AvailabilityType {
}

#FuncThatCallsCode("""
  if #available(macOS 12.0, *) {
    onlyInMacOS12()
  }
  onlyInMacOS12()
  """)
// expected-note@-6 3{{in expansion of macro 'FuncThatCallsCode' here}}
/*
expected-expansion@-8:1{{
  expected-note@1:6 {{add '@available' attribute to enclosing global function}}
  expected-error@5:3 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
  expected-note@5:3 {{add 'if #available' version check}}
}}
*/

struct HasAccessor {
  // expected-note@-1 {{add '@available' attribute to enclosing struct}}
  @GetterThatCallsCode("onlyInMacOS12()")
  // expected-note@-1 2{{in expansion of macro 'GetterThatCallsCode' on property 'value' here}}
  var value: Int
  // expected-note@-1 {{add '@available' attribute to enclosing property}}
  /*
  expected-expansion@-3:17{{
    expected-error@3:7 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
    expected-note@3:7 {{add 'if #available' version check}}
  }}
  */
}

// When the storage has @available, the accessor inherits it.
struct HasGatedAccessor {
  @available(macOS 12.0, *)
  @GetterThatCallsCode("onlyInMacOS12()")
  var value: Int
}

@BodyThatCallsCode("onlyInMacOS12()")
// expected-note@-1 {{in expansion of macro 'BodyThatCallsCode' on global function 'bodyMacroTest()' here}}
func bodyMacroTest() {
  // expected-note@-1 {{add '@available' attribute to enclosing global function}}
  /*
  expected-expansion@-3:22{{
    expected-error@2:5 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
  }}
  */
}

// When the function has @available, the body inherits it.
@available(macOS 12.0, *)
@BodyThatCallsCode("onlyInMacOS12()")
func gatedBodyMacroTest() {}

struct HasGatedMethod {
  // expected-note@-1 {{add '@available' attribute to enclosing struct}}
  @available(macOS 12.0, *)
  @PeerMethodThatCallsCode("onlyInMacOS12()")
  // expected-note@-1 3{{in expansion of macro 'PeerMethodThatCallsCode' on static method 'gatedMethod()' here}}
  static func gatedMethod() {}
  /*
  expected-expansion@-2:31{{
    expected-note@1:13 {{add '@available' attribute to enclosing static method}}
    expected-error@2:3 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
    expected-note@2:3 {{add 'if #available' version check}}
  }}
  */
}

@available(macOS 12.0, *)
@ExtensionMethodThatCallsCode("onlyInMacOS12()")
// expected-note@-1 6{{in expansion of macro 'ExtensionMethodThatCallsCode' on struct 'GatedTypeForExtension' here}}
struct GatedTypeForExtension {}
/*
expected-expansion@-2:32{{
  expected-note@1:1 2{{add '@available' attribute to enclosing extension}}
  expected-error@1:11 {{'GatedTypeForExtension' is only available in macOS 12.0 or newer}}
  expected-note@2:15 {{add '@available' attribute to enclosing static method}}
  expected-error@3:5 {{'onlyInMacOS12()' is only available in macOS 12.0 or newer}}
  expected-note@3:5 {{add 'if #available' version check}}
}}
*/
