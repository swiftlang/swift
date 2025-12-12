// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ClosureBodyMacro

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -enable-experimental-feature ClosureBodyMacro

@attached(body)
macro Empty() = #externalMacro(module: "MacroDefinition", type: "EmptyBodyMacro")

// Make sure we can handle the lack of 'in' here.
_ = { @Empty x -> // expected-error {{cannot infer type of closure parameter 'x' without a type annotation}}
  0 // expected-error {{expected closure result type after '->'}} expected-error {{expected 'in' after the closure signature}}
}

_ = { @Empty in }
