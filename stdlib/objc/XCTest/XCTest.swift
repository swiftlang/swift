//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@exported import XCTest // Clang module

/// Returns the current test case, so we can use free functions instead of methods for the overlay.
@asmname("_XCTCurrentTestCase") func _XCTCurrentTestCase() -> XCTestCase

/// Register the failure, expected or unexpected, of the current test case.
func _XCTRegisterFailure(expected: Bool, condition: String, message: String, file: String, line: UInt) -> Void {
  // Call the real _XCTFailureHandler.
  let test = _XCTCurrentTestCase()
  _XCTPreformattedFailureHandler(test, expected, file, line, condition, message)
}

/// Produce a failure description for the given assertion type.
func _XCTFailureDescription(assertionType: _XCTAssertionType, formatIndex: UInt, expressionStrings: CVarArg...) -> String {
  // In order to avoid revlock/submission issues between XCTest and the Swift XCTest overlay,
  // we are using the convention with _XCTFailureFormat that (formatIndex >= 100) should be
  // treated just like (formatIndex - 100), but WITHOUT the expression strings. (Swift can't
  // stringify the expressions, only their values.) This way we don't have to introduce a new
  // BOOL parameter to this semi-internal function and coordinate that across builds.
  //
  // Since there's a single bottleneck in the overlay where we invoke _XCTFailureFormat, just
  // add the formatIndex adjustment there rather than in all of the individual calls to this
  // function.
  
  return String(format: _XCTFailureFormat(assertionType, formatIndex + 100), arguments: expressionStrings)
}

// --- Supported Assertions ---

func XCTFail(_ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Fail
  
  _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, ""), message, file, line)
}

func XCTAssertNil(expression: @auto_closure () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Nil
  
  // evaluate the expression exactly once
  let expressionValueOptional: AnyObject? = expression()
  
  // test both Optional and value to treat .None and nil as synonymous
  var passed: Bool
  var expressionValueStr: String = "nil"
  if let expressionValueUnwrapped: AnyObject = expressionValueOptional {
    // TODO: passed = (expressionValueUnwrapped === nil)
    if expressionValueUnwrapped === nil {
      passed = true
    } else {
      passed = false
      expressionValueStr = reflect(expressionValueUnwrapped).summary
    }
  } else {
    passed = true
  }
  
  if !passed {
    // TODO: @auto_string expression
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertNotNil(expression: @auto_closure () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotNil
  
  // evaluate the expression exactly once
  let expressionValueOptional: AnyObject? = expression()
  
  // test both Optional and value to treat .None and nil as synonymous
  var passed: Bool
  var expressionValueStr: String = "nil"
  if let expressionValueUnwrapped: AnyObject = expressionValueOptional {
    // TODO: passed = !(expressionValueUnwrapped === nil)
    if expressionValueUnwrapped === nil {
      passed = false
    } else {
      passed = true
      expressionValueStr = reflect(expressionValueUnwrapped).summary
    }
  } else {
    passed = false
  }
  
  if !passed {
    // TODO: @auto_string expression
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssert(expression: @auto_closure () -> LogicValue, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  // XCTAssert is just a cover for XCTAssertTrue.
  XCTAssertTrue(expression, message, file: file, line: line);
}

func XCTAssertTrue(expression: @auto_closure () -> LogicValue, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.True
  
  // evaluate the expression exactly once
  let expressionValue = expression().getLogicValue()
  
  if !expressionValue {
    // TODO: @auto_string expression
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertFalse(expression: @auto_closure () -> LogicValue, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  let assertionType = _XCTAssertionType.False
  
  // evaluate the expression exactly once
  let expressionValue = expression().getLogicValue()
  
  if expressionValue {
    // TODO: @auto_string expression
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertEqual<T: Equatable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if expressionValue1 != expressionValue2 {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertNotEqual<T: Equatable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if expressionValue1 == expressionValue2 {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func _XCTCheckEqualWithAccuracy_Double(value1: Double, value2: Double, accuracy: Double) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_Float(value1: Float, value2: Float, accuracy: Float) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func XCTAssertEqualWithAccuracy<T: FloatingPointNumber>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.EqualWithAccuracy
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  var equalWithAccuracy: Bool = false
  
  switch (expressionValue1, expressionValue2, accuracy) {
  case let (expressionValue1Double as Double, expressionValue2Double as Double, accuracyDouble as Double):
    equalWithAccuracy = _XCTCheckEqualWithAccuracy_Double(expressionValue1Double, expressionValue2Double, accuracyDouble)
    
  case let (expressionValue1Float as Float, expressionValue2Float as Float, accuracyFloat as Float):
    equalWithAccuracy = _XCTCheckEqualWithAccuracy_Float(expressionValue1Float, expressionValue2Float, accuracyFloat)
    
  default:
    // unknown type, fail with prejudice
    _preconditionFailure("unsupported floating-point type passed to XCTAssertEqualWithAccuracy")
  }
  
  if !equalWithAccuracy {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    var accuracyStr = reflect(accuracy).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func _XCTCheckNotEqualWithAccuracy_Double(value1: Double, value2: Double, accuracy: Double) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_Float(value1: Float, value2: Float, accuracy: Float) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func XCTAssertNotEqualWithAccuracy<T: FloatingPointNumber>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqualWithAccuracy
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  var notEqualWithAccuracy: Bool = false
  
  switch (expressionValue1, expressionValue2, accuracy) {
  case let (expressionValue1Double as Double, expressionValue2Double as Double, accuracyDouble as Double):
    notEqualWithAccuracy = _XCTCheckNotEqualWithAccuracy_Double(expressionValue1Double, expressionValue2Double, accuracyDouble)
    
  case let (expressionValue1Float as Float, expressionValue2Float as Float, accuracyFloat as Float):
    notEqualWithAccuracy = _XCTCheckNotEqualWithAccuracy_Float(expressionValue1Float, expressionValue2Float, accuracyFloat)
    
  default:
    // unknown type, fail with prejudice
    _preconditionFailure("unsupported floating-point type passed to XCTAssertNotEqualWithAccuracy")
  }
  
  if !notEqualWithAccuracy {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    var accuracyStr = reflect(accuracy).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertGreaterThan<T: Comparable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.GreaterThan
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if !(expressionValue1 > expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertGreaterThanOrEqual<T: Comparable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.GreaterThanOrEqual
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if !(expressionValue1 >= expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertLessThan<T: Comparable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.LessThan
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if !(expressionValue1 < expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

func XCTAssertLessThanOrEqual<T: Comparable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.LessThanOrEqual
  
  // evaluate each expression exactly once
  let expressionValue1 = expression1()
  let expressionValue2 = expression2()
  
  if !(expressionValue1 <= expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = reflect(expressionValue1).summary
    let expressionValueStr2 = reflect(expressionValue2).summary
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  // TODO: handle an exception for which we can get a description
  // TODO: handle an exception for which we can't get a description
}

#if XCTEST_ENABLE_EXCEPTION_ASSERTIONS
// --- Currently-Unsupported Assertions ---

func XCTAssertThrows(expression: @auto_closure () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_Throws
  
  // FIXME: Unsupported
}

func XCTAssertThrowsSpecific(expression: @auto_closure () -> Any?, exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecific
  
  // FIXME: Unsupported
}

func XCTAssertThrowsSpecificNamed(expression: @auto_closure () -> Any?, exception: Any, name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecificNamed
  
  // FIXME: Unsupported
}

func XCTAssertNoThrow(expression: @auto_closure () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrow
  
  // FIXME: Unsupported
}

func XCTAssertNoThrowSpecific(expression: @auto_closure () -> Any?, exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecific
  
  // FIXME: Unsupported
}

func XCTAssertNoThrowSpecificNamed(expression: @auto_closure () -> Any?, exception: Any, name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecificNamed
  
  // FIXME: Unsupported
}
#endif
