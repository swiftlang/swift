// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ClosureBodyMacro

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath -swift-version 5

// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -enable-experimental-feature ClosureBodyMacro

@attached(body)
macro Empty() = #externalMacro(module: "MacroDefinition", type: "EmptyBodyMacro")

// Make sure we can handle the lack of 'in' here.
// swift-syntax only recognizes the attribute as part of a closure signature when
// it can see the 'in', so it parses '@Empty x' as the start of the body.
_ = { @Empty x -> // expected-cparser-error {{cannot infer type of closure parameter 'x' without a type annotation}}
  // expected-astgen-error@-1 {{expected declaration and ';' after attribute}}
  // expected-astgen-error@-2 {{cannot find 'x' in scope}}
  // expected-astgen-error@-3 {{expected type before '->'}}
  0 // expected-cparser-error {{expected closure result type after '->'}} expected-cparser-error {{expected 'in' after the closure signature}}
  // expected-astgen-error@-1 {{expected type after '->'}}
}

_ = { @Empty in }
