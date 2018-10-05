// RUN: %target-typecheck-verify-swift -DFLAG=42

#flagValue("FLAG") // expected-warning {{integer literal is unused}}

_ = #flagValue(99) // not-expected-error {{#flagValue argument must be string literal}}

_ = #flagValue("FLAG2")
// xpected-error@-1 {{#flagValue no value found for key 'FLAG2' and no default}}
