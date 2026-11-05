// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx
// This file is mostly an inverted version of availability_query.swift
if #unavailable(OSX 51) {
}

// Disallow explicit wildcards.
if #unavailable(OSX 51, *) {} // expected-error {{platform wildcard '*' is always implicit in #unavailable}} {{25-26=}}
// Disallow use as an expression.
if (#unavailable(OSX 51)) {}  // expected-cparser-error {{#unavailable may only be used as condition of an 'if', 'guard'}} expected-astgen-error {{availability condition cannot be used in an expression, only as a condition of 'if' or 'guard'}}
let x = #unavailable(OSX 51)  // expected-cparser-error {{#unavailable may only be used as condition of}} expected-astgen-error {{availability condition cannot be used in an expression, only as a condition of 'if' or 'guard'}}
(#unavailable(OSX 51) ? 1 : 0) // expected-cparser-error {{#unavailable may only be used as condition of an}} expected-astgen-error {{availability condition cannot be used in an expression, only as a condition of 'if' or 'guard'}}
if !#unavailable(OSX 52) { // expected-cparser-error {{#unavailable may only be used as condition of an}} expected-astgen-error {{availability condition cannot be used in an expression; did you mean '#available'?}}
}
if let _ = Optional(5), !#unavailable(OSX 52) { // expected-cparser-error {{#unavailable may only be used as condition}} expected-astgen-error {{availability condition cannot be used in an expression; did you mean '#available'?}}
}

if #unavailable(OSX 51) && #unavailable(OSX 52) { // expected-cparser-error {{expected ',' joining parts of a multi-clause condition}} {{24-27=,}} expected-astgen-error {{expected ',' joining parts of a multi-clause condition}}{{24-25=}}{{28-28=, }}{{25-28=}}
}


if #unavailable { // expected-cparser-error {{expected availability condition}} expected-astgen-error {{expected '(', '@availability' arguments, and ')' in availability condition}}
}

if #unavailable( { // expected-cparser-error {{expected platform name}} expected-cparser-error {{expected ')'}} expected-astgen-error {{expected platform and ')' to end availability condition}} expected-cparser-note {{to match this opening '('}}
}

if #unavailable() { // expected-cparser-error {{expected platform name}} expected-astgen-error {{expected version restriction in availability argument}}
}

if #unavailable(OSX { // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-astgen-error {{expected version number}}
}

if #unavailable(OSX) { // expected-error {{expected version number}}
}

if #unavailable(OSX 51 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(iDishwasherOS 51) { // expected-warning {{cannot find availability domain 'iDishwasherOS'}}
}

if #unavailable(iDishwasherOS 51) { // expected-warning {{cannot find availability domain 'iDishwasherOS'}}
}

if #unavailable(macos 51) { // expected-warning {{cannot find availability domain 'macos'; did you mean 'macOS'?}} {{17-22=macOS}}
}

if #unavailable(mscos 51) { // expected-warning {{cannot find availability domain 'mscos'; did you mean 'macOS'?}} {{17-22=macOS}}
}

if #unavailable(macoss 51) { // expected-warning {{cannot find availability domain 'macoss'; did you mean 'macOS'?}} {{17-23=macOS}}
}

if #unavailable(mac 51) { // expected-warning {{cannot find availability domain 'mac'; did you mean 'macOS'?}} {{17-20=macOS}}
}

if #unavailable(OSX 51, OSX 52) {  // expected-error {{version for macOS already specified}}
}

if #unavailable(OSX 51, iOS 8.0, *) { }  // expected-error {{platform wildcard '*' is always implicit in #unavailable}} {{34-35=}}
if #unavailable(iOS 8.0) {
}

if #unavailable(iOSApplicationExtension, unavailable) { // expected-cparser-error {{'unavailable' can't be combined with shorthand specification 'iOSApplicationExtension'}} expected-astgen-error {{expected version number}} expected-astgen-warning {{cannot find availability domain 'unavailable'}}
// expected-cparser-note@-1 {{did you mean to specify an introduction version?}}
}

// Should this be a valid spelling since `#unvailable(*)` cannot be written?
if #unavailable() { // expected-cparser-error {{expected platform name}} expected-astgen-error {{expected version restriction in availability argument}}
}

if #unavailable(OSX 10 { // expected-cparser-error {{expected ')' in availability query}} expected-astgen-error {{expected ')' to end availability condition}} expected-note {{to match this opening '('}}
}

// Multiple platforms
if #unavailable(OSX 51, iOS 8.0) {
}


if #unavailable(OSX 51, { // expected-cparser-error {{expected platform name}} expected-astgen-error {{expected version restriction in availability argument}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(OSX 51, iOS { // expected-error {{expected ')'}} expected-note {{to match this opening '('}} expected-astgen-error {{expected version number}}
}

if #unavailable(OSX 51, iOS 8.0, iDishwasherOS 51) { // expected-warning {{cannot find availability domain 'iDishwasherOS'}}
}

if #unavailable(iDishwasherOS 51, OSX 51) { // expected-warning {{cannot find availability domain 'iDishwasherOS'}}
}

if #unavailable(OSX 51 || iOS 8.0) {// expected-cparser-error {{'||' cannot be used in an availability condition}} expected-astgen-error {{expected ',' joining platforms in availability condition}}
}

// Emit Fix-It removing un-needed >=, for the moment.
if #unavailable(OSX >= 51) { // expected-error {{version comparison not needed}} {{21-24=}}
}

// Bool then #unavailable.
if 1 != 2, #unavailable(iOS 8.0) {}

// Pattern then #unavailable(iOS 8.0) {
if case 42 = 42, #unavailable(iOS 8.0) {}
if let _ = Optional(42), #unavailable(iOS 8.0) {}

// Allow "macOS" as well.
if #unavailable(macOS 51) {
}

// Prevent availability and unavailability being present in the same statement.
if #unavailable(macOS 51), #available(macOS 52, *) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}
if #available(macOS 51, *), #unavailable(macOS 52) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}
if #available(macOS 51, *), #available(macOS 55, *), #unavailable(macOS 53) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}
if #unavailable(macOS 51), #unavailable(macOS 55), #available(macOS 53, *) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}
if case 42 = 42, #available(macOS 51, *), #unavailable(macOS 52) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}
if #available(macOS 51, *), case 42 = 42, #unavailable(macOS 52) { // expected-error {{#available and #unavailable cannot be in the same statement}}
}

// Allow availability and unavailability to mix if they are not in the same statement.
if #unavailable(macOS 51) {
  if #available(macOS 50, *) { }
}
if #available(macOS 50, *) {
  if #unavailable(macOS 51) { }
}

// Diagnose wrong spellings of unavailability
if #available(*) == false { // expected-cparser-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{4-14=#unavailable}} {{18-27=}} expected-astgen-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}}{{4-26=#unavailable(*)}}
}
if !#available(*) { // expected-cparser-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{4-15=#unavailable}} expected-astgen-error {{availability condition cannot be used in an expression; did you mean '#unavailable'?}}
} 
