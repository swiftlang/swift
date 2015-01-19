// RUN: %target-parse-verify-swift -enable-experimental-availability-checking

// REQUIRES: OS=macosx

if #os(OSX >= 10.10) {
}

// Allow parenthesized check
if (#os(OSX >= 10.10)) {
}

if #os { // expected-error {{expected availability condition}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #os( { // expected-error {{expected platform name}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #os() { // expected-error {{expected platform name}} expected-error {{check can only be used as guard of if statement}}
}

if #os(OSX { // expected-error {{expected version comparison}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #os(OSX) { // expected-error {{expected version comparison}} expected-error {{check can only be used as guard of if statement}}
}

if #os(OSX >=  { // expected-error {{expected version number}} expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-error {{check can only be used as guard of if statement}}
}

if #os(OSX >= ) { // expected-error {{expected version number}} expected-error {{check can only be used as guard of if statement}}
}

if #os(OSX >= 10.10 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #os(iDishwasherOS >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{check can only be used as guard of if statement}}
}


// Multiple platforms
if #os(OSX >= 10.10, iOS >= 8.0) {
}

if #os(OSX >= 10.10, { // expected-error {{expected platform name}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #os(OSX >= 10.10,) { // expected-error {{expected platform name}}
}

if #os(OSX >= 10.10, iOS { // expected-error {{expected version comparison}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #os(OSX >= 10.10, iOS >= 8.0, iDishwasherOS >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}}
}

if #os(iDishwasherOS >= 10.10, OSX >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}}
}

if #os(OSX >= 10.10 || iOS >= 8.0) { // expected-error {{'||' cannot be used in an availability condition}}
}
