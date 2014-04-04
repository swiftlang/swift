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

import XCTest


/// Register the failure, expected or unexpected, of the given test.
func _XCTRegisterFailure(test: XCTestCase, expected: Bool, condition: String, message: String, file: String, line: Int) -> Void {
  // Call the real _XCTFailureHandler.
  _XCTPreformattedFailureHandler(test, expected, file, line, condition, message)
}

/// Produce a failure description for the given assertion type.
func _XCTFailureDescription(assertionType: _XCTAssertionType, formatIndex: Int, expressionStrings: CVarArg...) -> String {
  return String(withFormat: _XCTFailureFormat(assertionType, formatIndex), arguments: expressionStrings)
}

extension XCTestCase {
  // --- Supported Assertions ---

  func XCTFail(message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_Fail

    _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, ""), message, file, line)
  }

  func XCTAssertNil(expression: @auto_closure () -> AnyObject?, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_Nil

    // evaluate the expression exactly once
    let expressionValueOptional: AnyObject? = expression()

    // test both Optional and value to treat .None and nil as synonymous
    var passed: Bool
    if let expressionValueUnwrapped: AnyObject = expressionValueOptional {
      // TODO: passed = (expressionValueUnwrapped === nil)
      if expressionValueUnwrapped === nil {
        passed = true
      } else {
        passed = false
      }
    } else {
      passed = true
    }

    if !passed {
      // TODO: @auto_string expression
      let expressionStr = "expressionStr"

      // TODO: stringify expressionValue
      let expressionValueStr = "expressionValueStr"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr, expressionValueStr), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertNotNil(expression: @auto_closure () -> AnyObject?, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NotNil

    // evaluate the expression exactly once
    let expressionValueOptional: AnyObject? = expression()

    // test both Optional and value to treat .None and nil as synonymous
    var passed: Bool
    if let expressionValueUnwrapped: AnyObject = expressionValueOptional {
      // TODO: passed = !(expressionValueUnwrapped === nil)
      if expressionValueUnwrapped === nil {
        passed = false
      } else {
        passed = true
      }
    } else {
      passed = false
    }

    if !passed {
      // TODO: @auto_string expression
      let expressionStr = "expressionStr"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssert(expression: @auto_closure () -> LogicValue, message: String = "", file: String = __FILE__, line: Int = __LINE__)  -> Void {
    // XCTAssert is just a cover for XCTAssertTrue.
    XCTAssertTrue(expression, message, file, line);
  }

  func XCTAssertTrue(expression: @auto_closure () -> LogicValue, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_True

    // evaluate the expression exactly once
    let expressionValue = expression().getLogicValue()

    if !expressionValue {
      // TODO: @auto_string expression
      let expressionStr = "expressionStr"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertFalse(expression: @auto_closure () -> LogicValue, message: String = "", file: String = __FILE__, line: Int = __LINE__)  -> Void {
    let assertionType = _XCTAssertionType.Assertion_False

    // evaluate the expression exactly once
    let expressionValue = expression().getLogicValue()

    if expressionValue {
      // TODO: @auto_string expression
      let expressionStr = expressionValue ? "true" : "false"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertEqualObjects(expression1: @auto_closure () -> NSObject, expression2: @auto_closure () -> NSObject, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_EqualObjects

    // evaluate each expression exactly once
    let expressionValue1 = expression1()
    let expressionValue2 = expression2()

    if (expressionValue1 == nil && expressionValue2 == nil) || !(expressionValue1.isEqual(expressionValue2)) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      let expressionStr1 = "expressionStr1"
      let expressionStr2 = "expressionStr2"

      // TODO: stringify expressionValue1
      // TODO: stringify expressionValue2
      let expressionValueStr1 = "expressionValueStr1"
      let expressionValueStr2 = "expressionValueStr2"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, expressionValueStr1, expressionValueStr2), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertNotEqualObjects(expression1: @auto_closure () -> NSObject, expression2: @auto_closure () -> NSObject, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NotEqualObjects

    // evaluate each expression exactly once
    let expressionValue1 = expression1()
    let expressionValue2 = expression2()

    if (expressionValue1 == expressionValue2) || expressionValue1.isEqual(expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      let expressionStr1 = "expressionStr1"
      let expressionStr2 = "expressionStr2"

      // TODO: stringify expressionValue1
      // TODO: stringify expressionValue2
      let expressionValueStr1 = "expressionValueStr1"
      let expressionValueStr2 = "expressionValueStr2"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, expressionValueStr1, expressionValueStr2), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertEqual<T: Equatable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_Equal

    // evaluate each expression exactly once
    let expressionValue1 = expression1()
    let expressionValue2 = expression2()

    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      let expressionStr1 = "expressionStr1"
      let expressionStr2 = "expressionStr2"

      // TODO: stringify expressionValue1
      // TODO: stringify expressionValue2
      let expressionValueStr1 = "expressionValueStr1"
      let expressionValueStr2 = "expressionValueStr2"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, expressionValueStr1, expressionValueStr2), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertNotEqual<T: Equatable>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NotEqual

    // evaluate each expression exactly once
    let expressionValue1 = expression1()
    let expressionValue2 = expression2()

    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      let expressionStr1 = "expressionStr1"
      let expressionStr2 = "expressionStr2"

      // TODO: stringify expressionValue1
      // TODO: stringify expressionValue2
      let expressionValueStr1 = "expressionValueStr1"
      let expressionValueStr2 = "expressionValueStr2"

      _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, expressionValueStr1, expressionValueStr2), message, file, line)
    }

    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

#if XCTEST_ENABLE_EQUAL_WITH_ACCURACY_ASSERTIONS
  func XCTAssertEqualWithAccuracy<T: FloatingPointNumber>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, accuracy: T, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_EqualWithAccuracy

    // evaluate each expression exactly once
    let expressionValue1: FloatingPointNumber = expression1()
    let expressionValue2: FloatingPointNumber = expression2()

    if expressionValue1.isNaN() || expressionValue2.isNaN()
      || ((max(expressionValue1, expressionValue2)
        - min(expressionValue1, expressionValue2)) > accuracy) {
          // TODO: @auto_string expression1
          // TODO: @auto_string expression2
          let expressionStr1 = "expressionStr1"
          let expressionStr2 = "expressionStr2"

          // TODO: stringify expressionValue1
          // TODO: stringify expressionValue2
          // TODO: stringify accuracy
          let expressionValueStr1 = "expressionValueStr1"
          let expressionValueStr2 = "expressionValueStr2"
          let accuracyStr = "accuracyStr"

          _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, accuracyStr, expressionValueStr1, expressionValueStr2, accuracyStr), message, file, line)
    }
    
    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }

  func XCTAssertNotEqualWithAccuracy<T: FloatingPointNumber>(expression1: @auto_closure () -> T, expression2: @auto_closure () -> T, accuracy: T, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NotEqualWithAccuracy

    // evaluate each expression exactly once
    let expressionValue1: FloatingPointNumber = expression1()
    let expressionValue2: FloatingPointNumber = expression2()

    if !expressionValue1.isNaN() && !expressionValue2.isNaN()
      && ((max(expressionValue1, expressionValue2)
        - min(expressionValue1, expressionValue2)) <= accuracy) {
          // TODO: @auto_string expression1
          // TODO: @auto_string expression2
          // TODO: @auto_string accuracy
          let expressionStr1 = "expressionStr1"
          let expressionStr2 = "expressionStr2"

          // TODO: stringify expressionValue1
          // TODO: stringify expressionValue2
          // TODO: stringify accuracy
          let expressionValueStr1 = "expressionValueStr1"
          let expressionValueStr2 = "expressionValueStr2"
          let accuracyStr = "accuracyStr"

          _XCTRegisterFailure(self, true, _XCTFailureDescription(assertionType, 0, expressionStr1, expressionStr2, accuracyStr, expressionValueStr1, expressionValueStr2, accuracyStr), message, file, line)
    }
    
    // TODO: handle an exception for which we can get a description
    // TODO: handle an exception for which we can't get a description
  }
#endif

#if XCTEST_ENABLE_EXCEPTION_ASSERTIONS
  // --- Currently-Unsupported Assertions ---

  func XCTAssertThrows(expression: @auto_closure () -> Any?, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_Throws

    // FIXME: Unsupported
  }

  func XCTAssertThrowsSpecific(expression: @auto_closure () -> Any?, exception: Any, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_ThrowsSpecific

    // FIXME: Unsupported
  }

  func XCTAssertThrowsSpecificNamed(expression: @auto_closure () -> Any?, exception: Any, name: String, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_ThrowsSpecificNamed

    // FIXME: Unsupported
  }

  func XCTAssertNoThrow(expression: @auto_closure () -> Any?, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NoThrow

    // FIXME: Unsupported
  }

  func XCTAssertNoThrowSpecific(expression: @auto_closure () -> Any?, exception: Any, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NoThrowSpecific

    // FIXME: Unsupported
  }

  func XCTAssertNoThrowSpecificNamed(expression: @auto_closure () -> Any?, exception: Any, name: String, message: String = "", file: String = __FILE__, line: Int = __LINE__) -> Void {
    let assertionType = _XCTAssertionType.Assertion_NoThrowSpecificNamed

    // FIXME: Unsupported
  }
#endif
}
