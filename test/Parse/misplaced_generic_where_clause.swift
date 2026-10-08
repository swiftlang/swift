// RUN: %target-typecheck-verify-swift -parse -verify-additional-prefix legacy-
// RUN: %target-typecheck-verify-swift -parse -enable-experimental-feature ParserASTGen -verify-additional-prefix astgen-

// REQUIRES: swift_feature_ParserASTGen

protocol P {
  associatedtype Element
}

func misplaced<C: P>(_ value: C) where C.Element == Int -> String {} // expected-legacy-error {{'where' clause must follow the function's return type}} {{34-66=-> String where C.Element == Int}} expected-legacy-error {{single argument function types require parentheses}} expected-astgen-error {{'where' clause must follow the function's return type}} expected-astgen-note {{move 'where' clause after return type}} expected-astgen-error {{expected '(' to start function type}} expected-astgen-note {{insert '('}} expected-astgen-error {{expected ')' in function type}} expected-astgen-note {{insert ')'}}

func multipleRequirements<C: P>(_ value: C) where C: P, C.Element == Int -> String {} // expected-legacy-error {{'where' clause must follow the function's return type}} {{45-83=-> String where C: P, C.Element == Int}} expected-legacy-error {{single argument function types require parentheses}} expected-astgen-error {{'where' clause must follow the function's return type}} expected-astgen-note {{move 'where' clause after return type}} expected-astgen-error {{expected '(' to start function type}} expected-astgen-note {{insert '('}} expected-astgen-error {{expected ')' in function type}} expected-astgen-note {{insert ')'}}

func functionTypeRequirement<C: P>(_ value: C) where C.Element == (Int) -> String {}

func existingReturn<C: P>(_ value: C) -> Void where C.Element == Int -> String {} // expected-legacy-error {{single argument function types require parentheses}} expected-astgen-error {{expected '(' to start function type}} expected-astgen-note {{insert '('}} expected-astgen-error {{expected ')' in function type}} expected-astgen-note {{insert ')'}}
