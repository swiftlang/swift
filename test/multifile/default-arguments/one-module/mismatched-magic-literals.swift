// RUN: %target-swift-frontend -emit-sil -verify -module-name main -primary-file %s %S/Inputs/mismatched-magic-literals-other.swift

//
// Basic functionality
//

// We should warn when we we pass a `#function`-defaulted argument to a
// `#file`-defaulted argument.
func bad(function: String = #function) {
  // expected-note@-1 {{did you mean for parameter 'function' to default to '#file'?}} {{29-38=#file}}
  callee(file: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{16-16=(}} {{24-24=)}}
}

// We should not warn when we pass a `#file`-defaulted argument to a
// `#file`-defaulted argument.
func good(file: String = #file) {
  callee(file: file)
}

// We should not warn when we surround the `#function`-defaulted argument
// with parentheses, which explicitly silences the warning.
func disabled(function: String = #function) {
  callee(file: (function))
}

//
// Looking through implicit conversions
//
// Same as above, but these lift the argument from `String` to `String?`, so
// the compiler has to look through the implicit conversion.
//

// We should warn when we we pass a `#function`-defaulted argument to a
// `#file`-defaulted argument.
func optionalBad(function: String = #function) {
  // expected-note@-1 {{did you mean for parameter 'function' to default to '#file'?}} {{37-46=#file}}
  callee(optFile: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'optFile', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{19-19=(}} {{27-27=)}}
}

// We should not warn when we pass a `#file`-defaulted argument to a
// `#file`-defaulted argument.
func optionalGood(file: String = #file) {
  callee(optFile: file)
}

// We should not warn when we surround the `#function`-defaulted argument
// with parentheses, which explicitly silences the warning.
func optionalDisabled(function: String = #function) {
  callee(optFile: (function))
}

//
// More negative cases
//

// We should not warn if the caller's parameter has no default.
func explicit(arbitrary: String) {
  callee(file: arbitrary)
}

// We should not warn if the caller's parameter has a non-magic-identifier
// default.
func empty(arbitrary: String = "") {
  callee(file: arbitrary)
}

// We should not warn if the callee's parameter has no default.
func ineligible(function: String = #function) {
  callee(arbitrary: function)
}
