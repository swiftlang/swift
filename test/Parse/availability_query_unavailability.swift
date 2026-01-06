// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=macosx
// This file is mostly an inverted version of availability_query.swift
if #unavailable(OSX 51) {
}

// Disallow explicit wildcards.
if #unavailable(OSX 51, *) {} // expected-error {{platform wildcard '*' is always implicit in #unavailable}} {{25-26=}}
// Disallow use as an expression.
if (#unavailable(OSX 51)) {}  // expected-error {{#unavailable may only be used as condition of an 'if', 'guard'}}
let x = #unavailable(OSX 51)  // expected-error {{#unavailable may only be used as condition of}}
(#unavailable(OSX 51) ? 1 : 0) // expected-error {{#unavailable may only be used as condition of an}}
if !#unavailable(OSX 52) { // expected-error {{#unavailable may only be used as condition of an}}
}
if let _ = Optional(5), !#unavailable(OSX 52) { // expected-error {{#unavailable may only be used as condition}}
}

if #unavailable(OSX 51) && #unavailable(OSX 52) { // expected-error {{expected ',' joining parts of a multi-clause condition}} {{24-27=,}}
}


if #unavailable { // expected-error {{expected availability condition}}
}

if #unavailable( { // expected-error {{expected platform name}} expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable() { // expected-error {{expected platform name}}
}

if #unavailable(OSX { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(OSX) { // expected-error {{expected version number}}
}

if #unavailable(OSX 51 { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(iDishwasherOS 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
}

if #unavailable(iDishwasherOS 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
}

if #unavailable(macos 51) { // expected-warning {{unrecognized platform name 'macos'; did you mean 'macOS'?}} {{17-22=macOS}}
}

if #unavailable(mscos 51) { // expected-warning {{unrecognized platform name 'mscos'; did you mean 'macOS'?}} {{17-22=macOS}}
}

if #unavailable(macoss 51) { // expected-warning {{unrecognized platform name 'macoss'; did you mean 'macOS'?}} {{17-23=macOS}}
}

if #unavailable(mac 51) { // expected-warning {{unrecognized platform name 'mac'; did you mean 'macOS'?}} {{17-20=macOS}}
}

if #unavailable(OSX 51, OSX 52) {  // expected-error {{version for macOS already specified}}
}

if #unavailable(OSX 51, iOS 8.0, *) { }  // expected-error {{platform wildcard '*' is always implicit in #unavailable}} {{34-35=}}
if #unavailable(iOS 8.0) {
}

if #unavailable(iOSApplicationExtension, unavailable) { // expected-error {{'unavailable' can't be combined with shorthand specification 'iOSApplicationExtension'}}
// expected-note@-1 {{did you mean to specify an introduction version?}}
}

// Should this be a valid spelling since `#unvailable(*)` cannot be written?
if #unavailable() { // expected-error {{expected platform name}}
}

if #unavailable(OSX 10 { // expected-error {{expected ')' in availability query}} expected-note {{to match this opening '('}}
}

// Multiple platforms
if #unavailable(OSX 51, iOS 8.0) {
}


if #unavailable(OSX 51, { // expected-error {{expected platform name}} // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(OSX 51, iOS { // expected-error {{expected ')'}} expected-note {{to match this opening '('}}
}

if #unavailable(OSX 51, iOS 8.0, iDishwasherOS 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
}

if #unavailable(iDishwasherOS 51, OSX 51) { // expected-warning {{unrecognized platform name 'iDishwasherOS'}}
}

if #unavailable(OSX 51 || iOS 8.0) {// expected-error {{'||' cannot be used in an availability condition}}
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
if #available(*) == false { // expected-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{4-14=#unavailable}} {{18-27=}}
}
if !#available(*) { // expected-error {{#available cannot be used as an expression, did you mean to use '#unavailable'?}} {{4-15=#unavailable}}
} 
