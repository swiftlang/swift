// Declarations named with the reserved '$' prefix, checked against both
// parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func $declareWithDollar() { // expected-error{{cannot declare entity named '$declareWithDollar'}}
  func $bar() { } // expected-error{{cannot declare entity named '$bar'}}
  func wibble(
    $a: Int, // expected-error{{cannot declare entity named '$a'}}
    $b c: Int) { } // expected-error{{cannot declare entity named '$b'}}
  let ($a: _, _) = (0, 0) // expected-error{{cannot declare entity named '$a'}}
  class $Class {} // expected-error{{cannot declare entity named '$Class'; the '$' prefix is reserved}}
}

protocol $Protocol {} // expected-error {{cannot declare entity named '$Protocol'; the '$' prefix is reserved}}

func `$declareEscapedWithDollar`() { } // expected-error{{cannot declare entity named '$declareEscapedWithDollar'}}
