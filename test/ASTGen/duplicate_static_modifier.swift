// A second 'static' or 'class' modifier is diagnosed by both parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library
// RUN: %target-typecheck-verify-swift -swift-version 5 -parse-as-library \
// RUN:   -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

class C {
  class static func f() {} // expected-error {{'static' cannot appear after another 'static' or 'class' keyword}} {{9-16=}}
  static class var v: Int { 1 } // expected-error {{'class' cannot appear after another 'static' or 'class' keyword}} {{10-16=}}
  class static subscript(a: Float) -> Int { 42 } // expected-error {{'static' cannot appear after another 'static' or 'class' keyword}} {{9-16=}}
}
