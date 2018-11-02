// RUN: %target-typecheck-verify-swift

_ = 1..<1 // OK
_ = 1…1 // expected-error {{use of unresolved operator '…'; did you mean '...'?}} {{6-9=...}}
_ = 1….1 // expected-error {{use of unresolved operator '…'; did you mean '...'?}} {{6-9=...}}
_ = 1.…1 // expected-error {{use of unresolved operator '.…'; did you mean '...'?}} {{6-10=...}}
_ = 1…<1 // expected-error {{use of unresolved operator '…<'; did you mean '..<'?}} {{6-10=..<}}
_ = 1..1 // expected-error {{use of unresolved operator '..'; did you mean '...'?}} {{6-8=...}}
_ = 1....1 // expected-error {{use of unresolved operator '....'; did you mean '...'?}} {{6-10=...}}
_ = 1...<1 // expected-error {{use of unresolved operator '...<'; did you mean '..<'?}} {{6-10=..<}}
_ = 1....<1 // expected-error {{use of unresolved operator '....<'; did you mean '..<'?}} {{6-11=..<}}

var i = 1
i++ // expected-error {{use of unresolved operator '++'; did you mean '+= 1'?}}
++i // expected-error {{use of unresolved operator '++'; did you mean '+= 1'?}}
i-- // expected-error {{use of unresolved operator '--'; did you mean '-= 1'?}}
--i // expected-error {{use of unresolved operator '--'; did you mean '-= 1'?}}

var d = 1.0
d++ // expected-error {{use of unresolved operator '++'; did you mean '+= 1'?}}
++d // expected-error {{use of unresolved operator '++'; did you mean '+= 1'?}}
d-- // expected-error {{use of unresolved operator '--'; did you mean '-= 1'?}}
--d // expected-error {{use of unresolved operator '--'; did you mean '-= 1'?}}
