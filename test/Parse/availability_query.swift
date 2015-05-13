// RUN: %target-parse-verify-swift

// REQUIRES: OS=macosx

if #available(OSX 10.10, *) {
}

// Disallow use as an expression.
if (#available(OSX 10.10, *)) {}  // expected-error {{#available may only be used as condition of an 'if', 'guard'}}

let x = #available(OSX 10.10, *)  // expected-error {{#available may only be used as condition of}}

(#available(OSX 10.10) ? 1 : 0) // expected-error {{#available may only be used as condition of an}}

if !#available(OSX 10.11, *) { // expected-error {{#available may only be used as condition of an}}
}
if let _ = Optional(5) where !#available(OSX 10.11, *) { // expected-error {{#available may only be used as condition}}
}

if #available(OSX 10.10) && #available(OSX 10.11) { // expected-error {{expected '{' after 'if' condition}} expected-error 3 {{}} expected-note {{}}
}


if #available { // expected-error {{expected availability condition}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #available( { // expected-error {{expected platform name}} expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available() { // expected-error {{expected platform name}}
}

if #available(OSX { // expected-error {{expected version number}} expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available(OSX) { // expected-error {{expected version number}}
}

if #available(OSX 10.10 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}}
}

if #available(iDishwasherOS 10.10, *) { // expected-error {{unrecognized platform name 'iDishwasherOS'}}
}

if #available(iOS 8.0, *) {
}

// Want to make sure we can parse this. Perhaps we should not let this validate, though.
if #available(*) {
}

if #available(* { // expected-error {{expected ')' in availability query}} expected-note {{to match this opening '('}}
}

// Multiple platforms
if #available(OSX 10.10, iOS 8.0, *) {
}

if #available(OSX 10.10, iOS 8.0) { // expected-error {{check must handle potential future platforms with '*'}}
}


if #available(OSX 10.10, { // expected-error {{expected platform name}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX 10.10,) { // expected-error {{expected platform name}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX 10.10, iOS { // expected-error {{expected version number}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX 10.10, iOS 8.0, iDishwasherOS 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS 10.10, OSX 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check must handle potential future platforms with '*'}}
}

if #available(OSX 10.10 || iOS 8.0) {// expected-error {{'||' cannot be used in an availability condition}} expected-error {{check must handle potential future platforms with '*'}}
}

// Emit Fix-It removing un-needed >=, for the moment.

if #available(OSX >= 10.10, *) { // expected-error {{version comparison not needed}} {{19-21=}}
}

// <rdar://problem/20904820> Following a "let" condition with #available is incorrectly rejected

// Bool then #available.
if 1 != 2, #available(iOS 8.0, *) {}

// Pattern then #available(iOS 8.0, *) {
if case 42 = 42, #available(iOS 8.0, *) {}
if let x = Optional(42), #available(iOS 8.0, *) {}



