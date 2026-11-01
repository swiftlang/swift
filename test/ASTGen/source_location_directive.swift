// Checks on '#sourceLocation' and the legacy '#line' directive, shared by both
// parsers.

// RUN: %target-typecheck-verify-swift -swift-version 5
// RUN: %target-typecheck-verify-swift -swift-version 5 \
// RUN:   -enable-experimental-feature ParserASTGen -verify-additional-prefix astgen-

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

let x = 0

#sourceLocation(file: "a.swift", line: 10)
#sourceLocation()

#sourceLocation(file: "b.swift", line: 0) // expected-error {{the line number needs to be greater than zero}}

// A closing directive with nothing open.
// expected-error@+2 {{parameterless closing #sourceLocation() directive without prior opening #sourceLocation(file:,line:) directive}}
#sourceLocation()
_ = x

// SwiftParser sees a '#line' literal followed by more expressions.
// expected-error@+5 {{#line directive was renamed to #sourceLocation}}
// expected-astgen-error@+4 2 {{consecutive statements on a line must be separated by newline or ';'}}
// expected-astgen-warning@+3 {{#line literal is unused}}
// expected-astgen-warning@+2 {{integer literal is unused}}
// expected-astgen-warning@+1 {{string literal is unused}}
#line 32000 "troops_on_the_water"
