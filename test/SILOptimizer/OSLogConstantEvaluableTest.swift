// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -primary-file %s -o %t/OSLogConstantEvaluableTest_silgen.sil
//
// Run the (mandatory) passes on which constant evaluator depends, and run the
// constant evaluator on the SIL produced after the dependent passes are run.
//
// RUN: %target-sil-opt -silgen-cleanup -raw-sil-inst-lowering -allocbox-to-stack -mandatory-inlining -constexpr-limit 2048 -test-constant-evaluable-subset %t/OSLogConstantEvaluableTest_silgen.sil > %t/OSLogConstantEvaluableTest.sil 2> %t/error-output
//
// RUN: %FileCheck %s < %t/error-output
//
// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos

// Test that the functions defined in the OSLogPrototype overlay annotated as
// constant evaluable are so (with the constexpr-limit defined above).
// This test is meant to catch regressions in the OSLog overlay implementation
// affecting the constant evaluability of functions that are expected to be so.

import OSLogPrototype

// CHECK-LABEL: @init(stringLiteral: String) -> OSLogMessage
// CHECK-NOT: error:
@_semantics("test_driver")
func osLogMessageStringLiteralInitTest() -> OSLogMessage {
  return "A string literal"
}

// CHECK-LABEL: @init(literalCapacity: Int, interpolationCount: Int) -> OSLogInterpolation
// CHECK-NOT: error:
// CHECK-LABEL: @appendLiteral(String) -> ()
// CHECK-NOT: error:
// CHECK-LABEL: @appendInterpolation(_: @autoclosure () -> Int, format: IntFormat, privacy: Privacy) -> ()
// CHECK-NOT: error:
// CHECK-LABEL: @appendLiteral(String) -> ()
// CHECK-NOT: error:
// CHECK-LABEL: @init(stringInterpolation: OSLogInterpolation) -> OSLogMessage
// CHECK-NOT: error:
@_semantics("test_driver")
func intValueInterpolationTest() -> OSLogMessage {
  return "An integer value \(10)"
}

// CHECK-LABEL: @init(literalCapacity: Int, interpolationCount: Int) -> OSLogInterpolation
// CHECK-NOT: error:
// CHECK-LABEL: @appendInterpolation(_: @autoclosure () -> String, privacy: Privacy) -> ()
// CHECK-NOT: error:
@_semantics("test_driver")
func stringValueInterpolationTest() -> OSLogMessage {
  return "A string value \("xyz")"
}
