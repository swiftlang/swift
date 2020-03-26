// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null -verify
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Tests for the diagnostics produced by the OSLogOptimization pass that
// performs compile-time analysis and optimization of the new os log prototype
// APIs. The tests here check whether bad user inputs are diagnosed correctly.
// The tests here model the possible invalid inputs to the os log methods.
// TODO: diagnostics will be improved. globalStringTablePointer builtin error
// must be suppressed.

import OSLogPrototype

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  // The following tests check the diagnostics for when the APIs are not invoked
  // with constants needed for generating a static format string.

  func testDynamicLogMessage(h: Logger, message: OSLogMessage) {
    // FIXME: log APIs must always be passed a string interpolation literal.
    // Diagnose this.
    h.log(level: .debug, message)
      // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNonconstantFormatOption(h: Logger, formatOpt: OSLogIntegerFormatting) {
    h.log(level: .debug, "Minimum integer value: \(Int.min, format: formatOpt)")
    // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
    // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNonconstantPrivacyOption(h: Logger,  privacyOpt: OSLogPrivacy) {
    h.log(level: .debug, "Minimum integer value: \(Int.min, privacy: privacyOpt)")
    // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
    // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNoninlinedOSLogMessage(h: Logger) {
    let logMessage: OSLogMessage = "Minimum integer value: \(Int.min)"
      // expected-error @-1 {{OSLogMessage instance must not be explicitly created and must be deletable}}
    h.log(level: .debug, logMessage)
  }

  func testNoninlinedOSLogMessageComplex(h: Logger, b: Bool) {
    let logMessage: OSLogMessage = "Maximum integer value: \(Int.max)"
      // expected-error @-1 {{OSLogMessage instance must not be explicitly created and must be deletable}}
    if !b {
      return
    }
    h.log(level: .debug, logMessage)
      // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNoninlinedFormatOptions(h: Logger) {
    let formatOption: OSLogIntegerFormatting = .hex(includePrefix: true)
    h.debug("Minimum integer value: \(Int.min, format: formatOption)")
      // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
      // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNoninlinedFormatOptionsComplex(h: Logger, b: Bool) {
    let formatOption: OSLogIntegerFormatting = .hex(includePrefix: true)
    if !b {
      return
    }
    h.debug("Minimum integer value: \(Int.min, format: formatOption)")
      // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
      // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
  }
}

internal enum Color {
  case red
  case blue
}

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  // Invoking the log calls in unreachable code should not crash the compiler.
  func testUnreachableLogCall(h: Logger, c: Color)  {
    let arg = 10
    switch c {
    case .red:
      return
    case .blue:
      return
    default: // expected-warning {{default will never be executed}}
      h.debug("Unreachable log call")
      h.info("Unreachable log call with argument \(arg)")
      h.log(
        """
        Unreachable log call with argument and formatting \
        \(arg, align: .right(columns: 10))
        """)
    }
  }

  // Passing InOut values to the logger should not crash the compiler.
  func foo(_ logger: Logger, _ mutableValue: inout String) {
     logger.log("FMFLabelledLocation: initialized with coder \(mutableValue)")
      // expected-error@-1 {{escaping closure captures 'inout' parameter 'mutableValue'}}
      // expected-note@-3 {{parameter 'mutableValue' is declared 'inout'}}
      // expected-note@-3 {{captured here}}
  }
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

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  func testUnreachableLogCallComplex(h: Logger, c: Color)  {
    switch c {
    case .red:
      return
    case .blue:
      return
    default: // expected-warning {{default will never be executed}}
      h.info("Some call \(c)")
        // expected-warning@-1 {{os log call will never be executed and may have undiagnosed errors}}
        // expected-error@-2 {{globalStringTablePointer builtin must used only on string literals}}
    }
  }
}
