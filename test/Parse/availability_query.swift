// RUN: %swift -parse -target x86_64-apple-macosx10.9 -verify %s

if #os(OSX >= 10.10) {
}

// Allow parenthesized check
if (#os(OSX >= 10.10)) {
}

if #os { // expected-error {{expected availability condition}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #os() { // expected-error {{expected platform name}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}}expected-error {{type of expression is ambiguous without more context}}
}

if #os(OSX) { // expected-error {{expected version comparison}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #os(OSX >= ) { // expected-error {{expected version number}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}

if #os(OSX >= 10.10 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #os(iDishwasherOS >= 10.10) { // expected-error {{unrecognized platform name 'iDishwasherOS'}} expected-error {{braced block of statements is an unused closure}} expected-error {{statement cannot begin with a closure expression}} expected-note {{explicitly discard the result of the closure by assigning to '_'}} expected-error {{type of expression is ambiguous without more context}}
}
