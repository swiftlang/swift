// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null -verify
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Tests for the diagnostics produced by the OSLogOptimization pass that
// performs compile-time analysis and optimization of the new os log prototype
// APIs. The tests here check whether bad user inputs are diagnosed correctly.
// The tests here model the possible invalid inputs to the os log methods.
// TODO: diagnostics will be improved.

import OSLogPrototype

if #available(OSX 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *) {

  func testDynamicLogMessage(h: Logger, message: OSLogMessage) {
    // FIXME: log APIs must always be passed a string interpolation literal.
    // Diagnose this.
    h.log(level: .debug, message)
      // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNonconstantFormatOption(h: Logger, formatOpt: IntFormat) {
    h.log(level: .debug, "Minimum integer value: \(Int.min, format: formatOpt)")
    // expected-error @-1 {{interpolation arguments like format and privacy options must be constants}}
    // expected-error @-2 {{globalStringTablePointer builtin must used only on string literals}}
  }

  func testNonconstantPrivacyOption(h: Logger,  privacyOpt: Privacy) {
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
      return;
    }
    h.log(level: .debug, logMessage)
      // expected-error @-1 {{globalStringTablePointer builtin must used only on string literals}}
  }
}

