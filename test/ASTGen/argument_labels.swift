// Argument-label warnings that the C++ parser emits while parsing parameters
// and call arguments, checked against both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func letVarLabel(var a: Int, let b: Int) {}
// expected-warning@-1 {{'var' in this position is interpreted as an argument label}} {{18-21=`var`}}
// expected-warning@-2 {{'let' in this position is interpreted as an argument label}} {{30-33=`let`}}

func doubledUp(a a: Int) {}
// expected-warning@-1 {{extraneous duplicate parameter name; 'a' already has an argument label}} {{16-18=}}

func underscoreTwice(_ _: Int) {}

func call(`in`: Int, `_`: Int) {}

func use() {
  call(`in`: 0, `_`: 1)
  // expected-warning@-1 {{keyword 'in' does not need to be escaped in argument list}} {{8-9=}} {{11-12=}}
}
