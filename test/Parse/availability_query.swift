// RUN: %target-typecheck-verify-swift

// REQUIRES: OS=macosx

if #available(OSX 51, *) {
}

// Disallow use as an expression.
if (#available(OSX 51, *)) {}  // expected-error {{#available may only be used as condition of an 'if', 'guard'}}

let x = #available(OSX 51, *)  // expected-error {{#available may only be used as condition of}}

(#available(OSX 51, *) ? 1 : 0) // expected-error {{#available may only be used as condition of an}}

if !#available(OSX 52, *) { // expected-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{4-15=#unavailable}}
}
if let _ = Optional(5), !#available(OSX 52, *) { // expected-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{25-36=#unavailable}}
}

if #available(OSX 51, *) && #available(OSX 52, *) { // expected-error {{expected ',' joining parts of a multi-clause condition}} {{25-28=,}}
}


if #available { // expected-error {{expected availability condition}}
}

if #available( { // expected-error {{expected platform name}} expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available() { // expected-error {{expected platform name}}
}

if #available(OSX { // expected-error {{expected version number}} expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available(OSX) { // expected-error {{expected version number}}
}

if #available(OSX 0) { // expected-warning {{expected version number; this is an error in the Swift 6 language mode}}
}

if #available(OSX 0.0) { // expected-warning {{expected version number; this is an error in the Swift 6 language mode}}
}

if #available(OSX 51 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{must handle potential future platforms with '*'}} {{21-21=, *}}
}

if #available(iDishwasherOS 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
// expected-error@-1 {{must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS 51, *) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
}

if #available(macos 51, *) { // expected-warning {{unrecognized platform name 'macos'; did you mean 'macOS'?}} {{15-20=macOS}}
}

if #available(mscos 51, *) { // expected-warning {{unrecognized platform name 'mscos'; did you mean 'macOS'?}} {{15-20=macOS}}
}

if #available(macoss 51, *) { // expected-warning {{unrecognized platform name 'macoss'; did you mean 'macOS'?}} {{15-21=macOS}}
}

if #available(mac 51, *) { // expected-warning {{unrecognized platform name 'mac'; did you mean 'macOS'?}} {{15-18=macOS}}
}

if #available(OSX 51, OSX 52, *) {  // expected-error {{version for 'macOS' already specified}}
}

if #available(OSX 52) { }  // expected-error {{must handle potential future platforms with '*'}} {{21-21=, *}}

if #available(OSX 51, iOS 8.0) { }  // expected-error {{must handle potential future platforms with '*'}} {{30-30=, *}}

if #available(iOS 8.0, *) {
}

if #available(iOSApplicationExtension, unavailable) { // expected-error 2{{expected version number}}
}
	
// Want to make sure we can parse this. Perhaps we should not let this validate, though.
if #available(*) {
}

if #available(* { // expected-error {{expected ')' in availability query}} expected-note {{to match this opening '('}}
}

// Multiple platforms
if #available(OSX 51, iOS 8.0, *) {
}


if #available(OSX 51, { // expected-error {{expected platform name}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available(OSX 51,) { // expected-error {{expected platform name}}
}

if #available(OSX 51, iOS { // expected-error {{expected version number}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #available(OSX 51, iOS 8.0, iDishwasherOS 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
// expected-error@-1 {{must handle potential future platforms with '*'}}
}

if #available(iDishwasherOS 51, OSX 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
// expected-error@-1 {{must handle potential future platforms with '*'}}
}

if #available(OSX 51 || iOS 8.0) {// expected-error {{'||' cannot be used in an availability condition}}
}

// Emit Fix-It removing un-needed >=, for the moment.

if #available(OSX >= 51, *) { // expected-error {{version comparison not needed}} {{19-22=}}
}

// Bool then #available.
if 1 != 2, #available(iOS 8.0, *) {}

// Pattern then #available(iOS 8.0, *) {
if case 42 = 42, #available(iOS 8.0, *) {}
if let _ = Optional(42), #available(iOS 8.0, *) {}

// Allow "macOS" as well.
if #available(macOS 51, *) {
}


