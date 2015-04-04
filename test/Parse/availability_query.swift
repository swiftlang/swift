// RUN: %target-parse-verify-swift -enable-experimental-availability-checking

// REQUIRES: OS=macosx

if #available(OSX >= 10.10, *) {
}

// Allow parenthesized check
if (#available(OSX >= 10.10, *)) {
}

if #available { // expected-error {{expected availability condition}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #available( { // expected-error {{expected platform name}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #available() { // expected-error {{expected platform name}} expected-error {{check can only be used as guard of if statement}}
}

if #available(OSX { // expected-error {{expected version comparison}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #available(OSX) { // expected-error {{expected version comparison}} expected-error {{check can only be used as guard of if statement}}
}

if #available(OSX >=  { // expected-error {{expected version number}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #available(OSX >= ) { // expected-error {{expected version number}} expected-error {{check can only be used as guard of if statement}}
}

if #available(OSX >= 10.10 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check can only be used as guard of if statement}}
}

if #available(iDishwasherOS >= 10.10, *) { // expected-error {{unrecognized platform name 'iDishwasherOS'}}
}

if #available(iOS >= 8.0, *) {
}

// Want to make sure we can parse this. Perhaps we should not let this validate, though.
if #available(*) {
}

if #available(* { // expected-error {{expected ')' in availability query}} expected-note {{to match this opening '('}}
}

// Multiple platforms
if #available(OSX >= 10.10, iOS >= 8.0, *) {
}

if #available(OSX >= 10.10, iOS >= 8.0) { // expected-error {{check must handle potential future platforms with '*'}}
}


if #available(OSX >= 10.10, { // expected-error {{expected platform name}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX >= 10.10,) { // expected-error {{expected platform name}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX >= 10.10, iOS { // expected-error {{expected version comparison}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX >= 10.10, iOS >= 8.0, iDishwasherOS >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS >= 10.10, OSX >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX >= 10.10 || iOS >= 8.0) { // expected-error {{'||' cannot be used in an availability condition}} expected-error {{check must handle potential future platforms with '*'}}
}
