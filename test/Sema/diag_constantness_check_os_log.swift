// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Tests the constantness Sema diagnostics for the OSLogTestHelper module,
// which acts as a stub for the os overlay.

import OSLogTestHelper

func testDynamicLogMessage(message: OSLogMessage) {
  _osLogTestHelper(message)
    // expected-error@-1 {{argument must be a string interpolation}}
}

func testNonconstantFormatOption(
  formatOpt: OSLogIntegerFormatting,
  explicitPositiveSign: Bool) {
  _osLogTestHelper("Minimum integer value: \(Int.min, format: formatOpt)")
    // expected-error@-1 {{argument must be a static method or property of 'OSLogIntegerFormatting'}}

  let uppercase = true
  _osLogTestHelper("\(UInt.max, format: .hex(uppercase: uppercase))")
    // expected-error@-1 {{argument must be a bool literal}}

  _osLogTestHelper("\(UInt.max, format: .hex)") // No error is expected here.
}

func testNonconstantPrivacyOption(privacyOpt: OSLogPrivacy) {
  _osLogTestHelper("Integer: \(Int.max, privacy: privacyOpt)")
    // expected-error@-1 {{argument must be a case of enum 'OSLogPrivacy'}}
}

func testNonconstantAlignmentOption(alignOpt: OSLogStringAlignment) {
  _osLogTestHelper("Integer: \(Int.max, align: alignOpt)")
    // expected-error@-1 {{argument must be a static method or property of 'OSLogStringAlignment'}}
}

func testMultipleOptions(
  formatOpt: OSLogIntegerFormatting,
  privacyOpt: OSLogPrivacy
) {
  _osLogTestHelper(
    """
    \(2, format: formatOpt, align: .right(columns: 10), privacy: privacyOpt)
    """)
    // expected-error@-2 {{argument must be a static method or property of 'OSLogIntegerFormatting'}}
    // expected-error@-3 {{argument must be a case of enum 'OSLogPrivacy'}}
}

func testNoninlinedOSLogMessage() {
  let logMessage: OSLogMessage = "Minimum integer value: \(Int.min)"
  _osLogTestHelper(logMessage)
    // expected-error@-1 {{argument must be a string interpolation}}
}

let globalLogMessage: OSLogMessage = "A global message"

func testGlobalLogMessage() {
  _osLogTestHelper(globalLogMessage)
    // expected-error@-1 {{argument must be a string interpolation}}
}

// No errors are expected here.
func testValidLogCalls(x: Int) {
  _osLogTestHelper("\(x, format: .hex, privacy: .private)")
  _osLogTestHelper("\(x, format: OSLogIntegerFormatting.hex, privacy: .public)")
  _osLogTestHelper("\(x, privacy: OSLogPrivacy.public)")
  _osLogTestHelper("\((x + 1) * 2, privacy: .public)")
}

// Check whether os-log-specific diagnostics do not crash when there
// are type errors.
func testTypeIncorrectLogCalls() {
  let message = "test message"

  _osLogTestHelper(message)
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
  _osLogTestHelper("prefix" + "\(x)")
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type 'OSLogMessage'}}
  _osLogTestHelper("prefix", "\(x)")
  // expected-error@-1 {{cannot convert value of type 'String' to expected argument type '(String, UnsafeBufferPointer<UInt8>) -> Void'}}
  // expected-error@-2 {{missing argument label 'assertion:' in call}}

  class TestClass {
  }
  let x = TestClass()
  _osLogTestHelper("\(x, format: .hex)")
  //expected-error@-1 {{no exact matches in call to instance method 'appendInterpolation'}}

  _osLogTestHelper("\(10, format: .myFormat, privacy: .private)")
  //expected-error@-1 {{type 'OSLogIntegerFormatting' has no member 'myFormat'}}
}

// Test diagnostics in extensions to OSLogInterpolation. This is not officially
// supported yet.
struct MyStruct {
  var i: Int
}

extension OSLogInterpolation {
  mutating func appendInterpolation(a: MyStruct) {
    self.appendInterpolation(a.i)
  }

  mutating func appendInterpolation(a: MyStruct, format: OSLogIntegerFormatting) {
    self.appendInterpolation(a.i, format: format)
      // expected-error@-1 {{argument must be a static method or property of 'OSLogIntegerFormatting'}}
  }

  @_semantics("oslog.requires_constant_arguments")
  mutating func appendInterpolation(a: MyStruct, constFormat: OSLogIntegerFormatting) {
    self.appendInterpolation(a.i, format: constFormat)
  }
}

func testOSLogInterpolationExtension(a: MyStruct) {
  // The following is not a Sema error but would result in SIL diagnostics as
  // the appendInterpolation overload is not marked as constant_evaluable.
  _osLogTestHelper("Error at line: \(a: a)")
}

func testExplicitOSLogMessageConstruction() {
  _osLogTestHelper(OSLogMessage(stringLiteral: "world"))
    // expected-error@-1 {{argument must be a string interpolation}}
  _osLogTestHelper(
    OSLogMessage( // expected-error {{argument must be a string interpolation}}
      stringInterpolation:
        OSLogInterpolation(
          literalCapacity: 0,
          interpolationCount: 0)))
}

// Test that @_semantics("oslog.log_with_level") permits values of type OSLog and
// OSLogType to not be constants.

class OSLog { }
class OSLogType { }

@_semantics("oslog.log_with_level")
func osLogWithLevel(_ level: OSLogType, log: OSLog, _ message: OSLogMessage) {
}

func testNonConstantLogObjectLevel(
  level: OSLogType,
  log: OSLog,
  message: OSLogMessage
) {
  osLogWithLevel(level, log: log, "message with no payload")
  var levelOpt: OSLogType? = nil
  levelOpt = level

  let logClosure = { log }
  osLogWithLevel(levelOpt!, log: logClosure(), "A string \("hello")")

  osLogWithLevel(level, log: log, message)
    // expected-error@-1 {{argument must be a string interpolation}}
}

// Test that log messages can be wrapped in constant_evaluable functions.

// A function similar to the one used by SwiftUI preview to wrap string
// literals.
@_semantics("constant_evaluable")
public func __designTimeStringStub(
  _ key: String,
  fallback: OSLogMessage
) -> OSLogMessage {
  fallback
}

func testSwiftUIPreviewWrapping() {
  // This should not produce any diagnostics.
  _osLogTestHelper(__designTimeStringStub("key", fallback: "A literal message"))
}

public func nonConstantFunction(
  _ key: String,
  fallback: OSLogMessage
) -> OSLogMessage {
  fallback
}

func testLogMessageWrappingDiagnostics() {
  _osLogTestHelper(nonConstantFunction("key", fallback: "A literal message"))
    // expected-error@-1{{argument must be a string interpolation}}
}
