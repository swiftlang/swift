// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// Make sure the diagnostic comes through...
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS

// Make sure the diagnostic doesn't crash in SILGen
// RUN: not %target-swift-frontend -swift-version 5 -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser -o - -g

@freestanding(expression) macro myWarning(_ message: String) = #externalMacro(module: "MacroDefinition", type: "WarningMacro")

@freestanding(expression) macro myError(_ message: String) = #externalMacro(module: "MacroDefinition", type: "ErrorMacro")

func testThrownError() {
  let name = "hello"
  #myWarning (name) // expected-error{{#myWarning macro requires a string literal (from macro 'myWarning')}}

  #myWarning("experimental features ahead") // expected-warning{{experimental features ahead}}
}

#if TEST_DIAGNOSTICS
func testThrownErrors() {
  let name = "hello"
  #myError(
    name
  ) // expected-error@-1{{macro requires a string literal (from macro 'myError')}}

  #myError("experimental features ahead") // expected-error{{experimental features ahead}}
}
#endif
