// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -enable-experimental-concise-pound-file

// The test cases in this file work the same in both Swift 5 and "Swift 6" mode.
// See the _swift5 and _swift6 files for version-specific test cases.

func callee(file: String = #file) {} // expected-note {{'file' declared here}}
func callee(optFile: String? = #file) {} // expected-note {{'optFile' declared here}}
func callee(fileID: String = #fileID) {} // expected-note {{'fileID' declared here}}
func callee(filePath: String = #filePath) {} // expected-note {{'filePath' declared here}}
func callee(arbitrary: String) {}

class SomeClass {
  static func callee(file: String = #file) {} // expected-note 2{{'file' declared here}}
  static func callee(optFile: String? = #file) {} // expected-note {{'optFile' declared here}}
  static func callee(arbitrary: String) {}

  func callee(file: String = #file) {} // expected-note 2{{'file' declared here}}
  func callee(optFile: String? = #file) {} // expected-note {{'optFile' declared here}}
  func callee(arbitrary: String) {}
}

//
// Basic functionality
//

// We should warn when we we pass a `#function`-defaulted argument to a
// `#file`-defaulted argument.
func bad(function: String = #function) {
  // expected-note@-1 3{{did you mean for parameter 'function' to default to '#file'?}} {{29-38=#file}}
  // expected-note@-2 {{did you mean for parameter 'function' to default to '#fileID'?}} {{29-38=#fileID}}
  // expected-note@-3 {{did you mean for parameter 'function' to default to '#filePath'?}} {{29-38=#filePath}}

  callee(file: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{16-16=(}} {{24-24=)}}

  SomeClass.callee(file: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{26-26=(}} {{34-34=)}}

  SomeClass().callee(file: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{28-28=(}} {{36-36=)}}

  callee(fileID: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'fileID', whose default argument is '#fileID'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{18-18=(}} {{26-26=)}}

  callee(filePath: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'filePath', whose default argument is '#filePath'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{20-20=(}} {{28-28=)}}
}

// We should not warn when we pass a `#file`-defaulted argument to a
// `#file`-defaulted argument.
func good(file: String = #file, fileID: String = #fileID, filePath: String = #filePath) {
  callee(file: file)

  SomeClass.callee(file: file)

  SomeClass().callee(file: file)

  callee(fileID: fileID)

  callee(filePath: filePath)
}

// We should not warn when we surround the `#function`-defaulted argument
// with parentheses, which explicitly silences the warning.
func disabled(function: String = #function) {
  callee(file: (function))

  SomeClass.callee(file: (function))

  SomeClass().callee(file: (function))

  callee(fileID: (function))

  callee(filePath: (function))
}

//
// With implicit instance `self`
//

extension SomeClass {
  // We should warn when we we pass a `#function`-defaulted argument to a
  // `#file`-defaulted argument.
  func bad(function: String = #function) {
    // expected-note@-1 1{{did you mean for parameter 'function' to default to '#file'?}} {{31-40=#file}}
    callee(file: function)
    // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
    // expected-note@-2 {{add parentheses to silence this warning}} {{18-18=(}} {{26-26=)}}
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
}

//
// With implicit type `self`
//

extension SomeClass {
  // We should warn when we we pass a `#function`-defaulted argument to a
  // `#file`-defaulted argument.
  static func bad(function: String = #function) {
    // expected-note@-1 1{{did you mean for parameter 'function' to default to '#file'?}} {{38-47=#file}}
    callee(file: function)
    // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'file', whose default argument is '#file'}}
    // expected-note@-2 {{add parentheses to silence this warning}} {{18-18=(}} {{26-26=)}}
  }

  // We should not warn when we pass a `#file`-defaulted argument to a
  // `#file`-defaulted argument.
  static func good(file: String = #file) {
    callee(file: file)
  }

  // We should not warn when we surround the `#function`-defaulted argument
  // with parentheses, which explicitly silences the warning.
  static func disabled(function: String = #function) {
    callee(file: (function))
  }
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
  // expected-note@-1 3{{did you mean for parameter 'function' to default to '#file'?}} {{37-46=#file}}
  callee(optFile: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'optFile', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{19-19=(}} {{27-27=)}}

  SomeClass.callee(optFile: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'optFile', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{29-29=(}} {{37-37=)}}

  SomeClass().callee(optFile: function)
  // expected-warning@-1 {{parameter 'function' with default argument '#function' passed to parameter 'optFile', whose default argument is '#file'}}
  // expected-note@-2 {{add parentheses to silence this warning}} {{31-31=(}} {{39-39=)}}
}

// We should not warn when we pass a `#file`-defaulted argument to a
// `#file`-defaulted argument.
func optionalGood(file: String = #file) {
  callee(optFile: file)

  SomeClass.callee(optFile: file)

  SomeClass().callee(optFile: file)
}

// We should not warn when we surround the `#function`-defaulted argument
// with parentheses, which explicitly silences the warning.
func optionalDisabled(function: String = #function) {
  callee(optFile: (function))

  SomeClass.callee(optFile: (function))

  SomeClass().callee(optFile: (function))
}

//
// More negative cases
//

// We should not warn if the caller's parameter has no default.
func explicit(arbitrary: String) {
  callee(file: arbitrary)

  SomeClass.callee(file: arbitrary)

  SomeClass().callee(file: arbitrary)
}

// We should not warn if the caller's parameter has a non-magic-identifier
// default.
func empty(arbitrary: String = "") {
  callee(file: arbitrary)

  SomeClass.callee(file: arbitrary)

  SomeClass().callee(file: arbitrary)
}

// We should not warn if the callee's parameter has no default.
func ineligible(function: String = #function) {
  callee(arbitrary: function)

  SomeClass.callee(arbitrary: function)

  SomeClass().callee(arbitrary: function)
}
