// RUN: %target-swift-frontend -swift-version 5 -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: VENDOR=apple

// Tests for the diagnostics produced by the OSLogOptimization pass that
// performs compile-time analysis and optimization of the new os log APIs.
// Note that many usage errors are caught by the Sema check: ConstantnessSemaDiagnostics.
// The tests here check only those diagnostics that are enforced at the SIL level.

import OSLogTestHelper

func testNonDecimalFormatOptionOnIntegers() {
  _osLogTestHelper("Minimum integer value: \(Int.min, format: .hex)")
  // expected-error @-1 {{Fatal error: Signed integers must be formatted using .decimal}}
}

// Extending OSLogInterpolation (without the constant_evaluable attribute) would be an
// error.
struct A {
  var i: Int
}
extension OSLogInterpolation {
  mutating func appendInterpolation(a: A) {
    self.appendInterpolation(a.i)
  }
}

func testOSLogInterpolationExtension(a: A) {
  _osLogTestHelper("Error at line: \(a: a)")
    // expected-error @-1 {{invalid log message; extending types defined in the os module is not supported}}
    // expected-note @-2 {{'OSLogInterpolation.appendLiteral(_:)' failed evaluation}}
    // expected-note @-3 {{value mutable by an unevaluated instruction is not a constant}}
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
  // expected-note@-1 {{parameter 'mutableValue' is declared 'inout'}}
   _osLogTestHelper("FMFLabelledLocation: initialized with coder \(mutableValue)")
    // expected-error@-1 {{escaping autoclosure captures 'inout' parameter 'mutableValue'}}
    // expected-note@-2 {{pass a copy of 'mutableValue'}}
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
      // expected-error@-2 {{globalStringTablePointer builtin must be used only on string literals}}
  }
}
