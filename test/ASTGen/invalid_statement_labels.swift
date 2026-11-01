// Labels on statements that cannot be labeled, checked against both parsers.

// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ParserASTGen

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

func f(_ x: Int) -> Int {
  a: return 1 // expected-error {{labels are only valid on loops, if, and switch statements}}
}

func g(_ b: Bool) {
  c: guard b else { return } // expected-error {{labels are only valid on loops, if, and switch statements}}
  d: defer {} // expected-error {{labels are only valid on loops, if, and switch statements}}
  e: for _ in 0..<1 { break e }
}
