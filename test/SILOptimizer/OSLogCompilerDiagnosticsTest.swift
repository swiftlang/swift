// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Tests for the diagnostics produced by the OSLogOptimization pass that
// performs compile-time analysis and optimization of the new os log prototype
// APIs. The tests here check whether bad user inputs are diagnosed correctly.
// The tests here model the possible invalid inputs to the os log methods.
// TODO: diagnostics will be improved. globalStringTablePointer builtin error
// must be suppressed.

import OSLogTestHelper

func testDynamicLogMessage(message: OSLogMessage) {
  _osLogTestHelper(message)
    // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
}

func testNonconstantFormatOption(formatOpt: OSLogIntegerFormatting) {
  _osLogTestHelper("Minimum integer value: \(Int.min, format: formatOpt)")
  // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
  // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
}

func testNonconstantPrivacyOption( privacyOpt: OSLogPrivacy) {
  _osLogTestHelper("Minimum integer value: \(Int.min, privacy: privacyOpt)")
  // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
  // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
}

func testNoninlinedOSLogMessage() {
  let logMessage: OSLogMessage = "Minimum integer value: \(Int.min)"
    // expected-error @-1 {{OSLogMessage instance must not be explicitly created and must be deletable}}
  _osLogTestHelper(logMessage)
}

func testNoninlinedOSLogMessageComplex(b: Bool) {
  let logMessage: OSLogMessage = "Maximum integer value: \(Int.max)"
    // expected-error @-1 {{OSLogMessage instance must not be explicitly created and must be deletable}}
  if !b {
    return
  }
  _osLogTestHelper(logMessage)
    // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
}

func testNoninlinedFormatOptions() {
  let formatOption: OSLogIntegerFormatting = .hex(includePrefix: true)
  _osLogTestHelper("Minimum integer value: \(Int.min, format: formatOption)")
    // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
    // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
}

func testNoninlinedFormatOptionsComplex(b: Bool) {
  let formatOption: OSLogIntegerFormatting = .hex(includePrefix: true)
  if !b {
    return
  }
  _osLogTestHelper("Minimum integer value: \(Int.min, format: formatOption)")
    // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
    // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
}

internal enum Color {
  case red
  case blue
}

// Invoking the log calls in unreachable code should not crash the compiler.
func testUnreachableLogCall(c: Color)  {
  let arg = 10
  switch c {
  case .red:
    return
  case .blue:
    return
  default: // expected-warning {{default will never be executed}}
    _osLogTestHelper("Unreachable log call")
    _osLogTestHelper("Unreachable log call with argument \(arg)")
    _osLogTestHelper(
      """
      Unreachable log call with argument and formatting \
      \(arg, align: .right(columns: 10))
      """)
  }
}

// Passing InOut values to the logger should not crash the compiler.
func foo(_ mutableValue: inout String) {
   _osLogTestHelper("FMFLabelledLocation: initialized with coder \(mutableValue)")
    // expected-error@-1 {{escaping closure captures 'inout' parameter 'mutableValue'}}
    // expected-note@-3 {{parameter 'mutableValue' is declared 'inout'}}
    // expected-note@-3 {{captured here}}
}

// This is an extension used only for testing a diagnostic that doesn't arise
// normally but may be triggered by changes to the library.
extension OSLogInterpolation {
  @_transparent
  mutating func appendInterpolation(_ c: Color) {
    switch c {
    case .red:
      appendInterpolation(1)
    case .blue:
      appendInterpolation(0)
    }
  }
}

func testUnreachableLogCallComplex(c: Color)  {
  switch c {
  case .red:
    return
  case .blue:
    return
  default: // expected-warning {{default will never be executed}}
    _osLogTestHelper("Some call \(c)")
      // expected-warning@-1 {{os log call will never be executed and may have undiagnosed errors}}
      // expected-error@-2 {{globalStringTablePointer builtin must used only on string literals}}
  }
}
