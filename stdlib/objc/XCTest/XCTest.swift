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

import CoreGraphics

/// Returns the current test case, so we can use free functions instead of methods for the overlay.
@asmname("_XCTCurrentTestCaseBridge") func _XCTCurrentTestCaseBridge() -> XCTestCase

// --- Failure Formatting ---

/// Register the failure, expected or unexpected, of the current test case.
func _XCTRegisterFailure(expected: Bool, condition: String, message: String, file: String, line: UInt) -> Void {
  // Call the real _XCTFailureHandler.
  let test = _XCTCurrentTestCaseBridge()
  _XCTPreformattedFailureHandler(test, expected, file, line, condition, message)
}

/// Produce a failure description for the given assertion type.
func _XCTFailureDescription(assertionType: _XCTAssertionType, formatIndex: UInt, expressionStrings: CVarArgType...) -> String {
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

// --- Exception Support ---

@asmname("_XCTRunThrowableBlockBridge") func _XCTRunThrowableBlockBridge(@objc_block () -> Void) -> NSDictionary

/// The Swift-style result of evaluating a block which may throw an exception.
enum _XCTThrowableBlockResult {
  case Success
  case FailedWithException(className: String, name: String, reason: String)
  case FailedWithUnknownException
}

/// Asks some Objective-C code to evaluate a block which may throw an exception,
/// and if it does consume the exception and return information about it.
func _XCTRunThrowableBlock(block: () -> Void) -> _XCTThrowableBlockResult {
  let d = _XCTRunThrowableBlockBridge(block) as [String:String]
  
  if d.count > 0 {
    let t = d["type"]!
    
    if t == "objc" {
      return .FailedWithException(className: d["className"]!, name: d["name"]!, reason: d["reason"]!)
    } else {
      return .FailedWithUnknownException
    }
  } else {
    return .Success
  }
}

// --- Supported Assertions ---

public func XCTFail(_ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Fail
  
  _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, "" as NSString), message, file, line)
}

public func XCTAssertNil(expression: @autoclosure () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Nil
  
  // evaluate the expression exactly once
  var expressionValueOptional: AnyObject?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression()
  }
  
  switch result {
  case .Success:
    // test both Optional and value to treat .None and nil as synonymous
    var passed: Bool
    var expressionValueStr: String = "nil"
    if let expressionValueUnwrapped: AnyObject! = expressionValueOptional {
      // TODO: passed = (expressionValueUnwrapped === nil)
      if expressionValueUnwrapped === nil {
        passed = true
      } else {
        passed = false
        expressionValueStr = "\(expressionValueUnwrapped)"
      }
    } else {
      passed = true
    }
    
    if !passed {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotNil(expression: @autoclosure () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotNil
  
  // evaluate the expression exactly once
  var expressionValueOptional: AnyObject?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression()
  }
  
  switch result {
  case .Success:
    // test both Optional and value to treat .None and nil as synonymous
    var passed: Bool
    var expressionValueStr: String = "nil"
    if let expressionValueUnwrapped: AnyObject! = expressionValueOptional {
      // TODO: passed = !(expressionValueUnwrapped === nil)
      if expressionValueUnwrapped === nil {
        passed = false
      } else {
        passed = true
        expressionValueStr = "\(expressionValueUnwrapped)"
      }
    } else {
      passed = false
    }
    
    if !passed {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message, file, line)
    }
    
  case .FailedWithException(let className, let name, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssert(expression: @autoclosure () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  // XCTAssert is just a cover for XCTAssertTrue.
  XCTAssertTrue(expression, message, file: file, line: line);
}

public func XCTAssertTrue(expression: @autoclosure () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.True
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression().boolValue
  }
  
  let expressionValue = expressionValueOptional!
  
  switch result {
  case .Success:
    if !expressionValue {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertFalse(expression: @autoclosure () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  let assertionType = _XCTAssertionType.False
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression().boolValue
  }
  
  let expressionValue = expressionValueOptional!
  
  switch result {
  case .Success:
    if expressionValue {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertEqual<T: Equatable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotEqual<T: Equatable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!

  switch result {
  case .Success:
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

func _XCTCheckEqualWithAccuracy_Double(value1: Double, value2: Double, accuracy: Double) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_Float(value1: Float, value2: Float, accuracy: Float) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_CGFloat(value1: CGFloat, value2: CGFloat, accuracy: CGFloat) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

public func XCTAssertEqualWithAccuracy<T: FloatingPointType>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.EqualWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    var equalWithAccuracy: Bool = false
    
    switch (expressionValue1, expressionValue2, accuracy) {
    case let (expressionValue1Double as Double, expressionValue2Double as Double, accuracyDouble as Double):
      equalWithAccuracy = _XCTCheckEqualWithAccuracy_Double(expressionValue1Double, expressionValue2Double, accuracyDouble)
      
    case let (expressionValue1Float as Float, expressionValue2Float as Float, accuracyFloat as Float):
      equalWithAccuracy = _XCTCheckEqualWithAccuracy_Float(expressionValue1Float, expressionValue2Float, accuracyFloat)
      
    case let (expressionValue1CGFloat as CGFloat, expressionValue2CGFloat as CGFloat, accuracyCGFloat as CGFloat):
      equalWithAccuracy = _XCTCheckEqualWithAccuracy_CGFloat(expressionValue1CGFloat, expressionValue2CGFloat, accuracyCGFloat)
      
    default:
      // unknown type, fail with prejudice
      _preconditionFailure("unsupported floating-point type passed to XCTAssertEqualWithAccuracy")
    }
    
    if !equalWithAccuracy {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      var accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

func _XCTCheckNotEqualWithAccuracy_Double(value1: Double, value2: Double, accuracy: Double) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_Float(value1: Float, value2: Float, accuracy: Float) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_CGFloat(value1: CGFloat, value2: CGFloat, accuracy: CGFloat) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

public func XCTAssertNotEqualWithAccuracy<T: FloatingPointType>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqualWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    var notEqualWithAccuracy: Bool = false
    
    switch (expressionValue1, expressionValue2, accuracy) {
    case let (expressionValue1Double as Double, expressionValue2Double as Double, accuracyDouble as Double):
      notEqualWithAccuracy = _XCTCheckNotEqualWithAccuracy_Double(expressionValue1Double, expressionValue2Double, accuracyDouble)
      
    case let (expressionValue1Float as Float, expressionValue2Float as Float, accuracyFloat as Float):
      notEqualWithAccuracy = _XCTCheckNotEqualWithAccuracy_Float(expressionValue1Float, expressionValue2Float, accuracyFloat)
      
    case let (expressionValue1CGFloat as CGFloat, expressionValue2CGFloat as CGFloat, accuracyCGFloat as CGFloat):
      notEqualWithAccuracy = _XCTCheckNotEqualWithAccuracy_CGFloat(expressionValue1CGFloat, expressionValue2CGFloat, accuracyCGFloat)
      
    default:
      // unknown type, fail with prejudice
      _preconditionFailure("unsupported floating-point type passed to XCTAssertNotEqualWithAccuracy")
    }
    
    if !notEqualWithAccuracy {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      var accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertGreaterThan<T: Comparable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.GreaterThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }

  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    if !(expressionValue1 > expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    var expressionValueStr1 = "\(expressionValue1)"
    let expressionValueStr2 = "\(expressionValue2)"
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertGreaterThanOrEqual<T: Comparable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.GreaterThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    if !(expressionValue1 >= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertLessThan<T: Comparable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.LessThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    if !(expressionValue1 < expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertLessThanOrEqual<T: Comparable>(expression1: @autoclosure () -> T, expression2: @autoclosure () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.LessThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  let expressionValue1 = expressionValue1Optional!
  let expressionValue2 = expressionValue2Optional!
  
  switch result {
  case .Success:
    if !(expressionValue1 <= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      var expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

#if XCTEST_ENABLE_EXCEPTION_ASSERTIONS
// --- Currently-Unsupported Assertions ---

public func XCTAssertThrows(expression: @autoclosure () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_Throws
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecific(expression: @autoclosure () -> Any?, exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecificNamed(expression: @autoclosure () -> Any?, exception: Any, name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecificNamed
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrow(expression: @autoclosure () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrow
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecific(expression: @autoclosure () -> Any?, exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecificNamed(expression: @autoclosure () -> Any?, exception: Any, name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecificNamed
  
  // FIXME: Unsupported
}
#endif
