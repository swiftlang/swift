// Source Input used by OSLogConstantEvaluableTest.swift and OSLogConstantEvaluableProfilerTest.swift
// Test that the functions defined in the OSLogPrototype overlay annotated as
// constant evaluable are so (with the constexpr-limit defined above).
// This test is meant to catch regressions in the OSLog overlay implementation
// affecting the constant evaluability of functions that are expected to be so.

import OSLogTestHelper

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
// CHECK-LABEL: @appendInterpolation(_: @autoclosure () -> Int, format: OSLogIntegerFormatting, align: OSLogStringAlignment, privacy: OSLogPrivacy) -> ()
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
// CHECK-LABEL: @appendInterpolation(_: @autoclosure () -> String, align: OSLogStringAlignment, privacy: OSLogPrivacy) -> ()
// CHECK-NOT: error:
@_semantics("test_driver")
func stringValueInterpolationTest() -> OSLogMessage {
  return "A string value \("xyz")"
}

@_semantics("test_driver")
func intValueWithPrecisionTest() -> OSLogMessage {
  return
    """
     An integer value \
     \(10, format: .decimal(minDigits: 25), align: .right(columns: 10))
    """
}

@_semantics("test_driver")
func intValueWithPrivacyTest() -> OSLogMessage {
    return "An integer value \(10, privacy: .private(mask: .hash))"
}

// Test OSLogMessage with SIMD interpolations
struct FloatVector {
    let zero = SIMD4<Float>()
    var pair: (SIMD4<Float>, SIMD4<Float>)

    init() {
        pair = (zero, zero)
    }
}

@_semantics("test_driver")
func testOSLogMessageSIMDInterpolation() -> OSLogMessage {
    let vector = FloatVector()
    return "\(vector.pair.0.x)"
}
