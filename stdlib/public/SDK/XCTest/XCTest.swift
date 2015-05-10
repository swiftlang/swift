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
func _XCTRegisterFailure(expected: Bool, _ condition: String, _ message: String, _ file: String, _ line: UInt) -> Void {
  // Call the real _XCTFailureHandler.
  let test = _XCTCurrentTestCaseBridge()
  _XCTPreformattedFailureHandler(test, expected, file, line, condition, message)
}

/// Produce a failure description for the given assertion type.
func _XCTFailureDescription(assertionType: _XCTAssertionType, _ formatIndex: UInt, _ expressionStrings: CVarArgType...) -> String {
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

@asmname("_XCTRunThrowableBlockBridge")
func _XCTRunThrowableBlockBridge(@noescape @convention(block) () -> Void) -> NSDictionary

/// The Swift-style result of evaluating a block which may throw an exception.
enum _XCTThrowableBlockResult {
  case Success
  case FailedWithException(className: String, name: String, reason: String)
  case FailedWithUnknownException
}

/// Asks some Objective-C code to evaluate a block which may throw an exception,
/// and if it does consume the exception and return information about it.
func _XCTRunThrowableBlock(@noescape block: () -> Void) -> _XCTThrowableBlockResult {
  let d = _XCTRunThrowableBlockBridge(block)
  
  if d.count > 0 {
    let t: String = d["type"] as! String
    
    if t == "objc" {
      return .FailedWithException(className: d["className"] as! String, name: d["name"] as! String, reason: d["reason"] as! String)
    } else {
      return .FailedWithUnknownException
    }
  } else {
    return .Success
  }
}

// --- Supported Assertions ---

public func XCTFail(message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Fail
  
  _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, "" as NSString), message, file, line)
}

public func XCTAssertNil(@autoclosure expression: () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
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
    if let expressionValueUnwrapped = expressionValueOptional {
      passed = false
      expressionValueStr = "\(expressionValueUnwrapped)"
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

public func XCTAssertNotNil(@autoclosure expression: () -> AnyObject?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
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
    if let expressionValueUnwrapped = expressionValueOptional {
      passed = true
      expressionValueStr = "\(expressionValueUnwrapped)"
    } else {
      passed = false
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

public func XCTAssert( @autoclosure expression: () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  // XCTAssert is just a cover for XCTAssertTrue.
  XCTAssertTrue(expression, message, file: file, line: line);
}

public func XCTAssertTrue(@autoclosure expression: () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.True
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression().boolValue
  }
  
  switch result {
  case .Success:
    let expressionValue = expressionValueOptional!
    
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

public func XCTAssertFalse(@autoclosure expression: () -> BooleanType, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)  -> Void {
  let assertionType = _XCTAssertionType.False
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = expression().boolValue
  }
  
  switch result {
  case .Success:
    let expressionValue = expressionValueOptional!
    
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

public func XCTAssertEqual<T : Equatable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

// FIXME: Due to <rdar://problem/16768059> we need overrides of XCTAssertEqual for:
//  ContiguousArray<T>
//  ArraySlice<T>
//  Array<T>
//  Dictionary<T, U>

public func XCTAssertEqual<T : Equatable>(@autoclosure expression1: () -> ArraySlice<T>, @autoclosure _ expression2: () -> ArraySlice<T>, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: ArraySlice<T>?
  var expressionValue2Optional: ArraySlice<T>?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: ArraySlice<T> = expressionValue1Optional!
    let expressionValue2: ArraySlice<T> = expressionValue2Optional!
    
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertEqual<T : Equatable>(@autoclosure expression1: () -> ContiguousArray<T>, @autoclosure _ expression2: () -> ContiguousArray<T>, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: ContiguousArray<T>?
  var expressionValue2Optional: ContiguousArray<T>?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: ContiguousArray<T> = expressionValue1Optional!
    let expressionValue2: ContiguousArray<T> = expressionValue2Optional!
    
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertEqual<T : Equatable>(@autoclosure expression1: () -> [T], @autoclosure _ expression2: () -> [T], _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: [T]?
  var expressionValue2Optional: [T]?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: [T] = expressionValue1Optional!
    let expressionValue2: [T] = expressionValue2Optional!
    
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertEqual<T, U : Equatable>(@autoclosure expression1: () -> [T: U], @autoclosure _ expression2: () -> [T: U], _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: [T: U]?
  var expressionValue2Optional: [T: U]?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: [T: U] = expressionValue1Optional!
    let expressionValue2: [T: U] = expressionValue2Optional!
    
    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotEqual<T : Equatable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

// FIXME: Due to <rdar://problem/16768059> we need overrides of XCTAssertNotEqual for:
//  ContiguousArray<T>
//  ArraySlice<T>
//  Array<T>
//  Dictionary<T, U>

public func XCTAssertNotEqual<T : Equatable>(@autoclosure expression1: () -> ContiguousArray<T>, @autoclosure _ expression2: () -> ContiguousArray<T>, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: ContiguousArray<T>?
  var expressionValue2Optional: ContiguousArray<T>?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: ContiguousArray<T> = expressionValue1Optional!
    let expressionValue2: ContiguousArray<T> = expressionValue2Optional!
    
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotEqual<T : Equatable>(@autoclosure expression1: () -> ArraySlice<T>, @autoclosure _ expression2: () -> ArraySlice<T>, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: ArraySlice<T>?
  var expressionValue2Optional: ArraySlice<T>?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: ArraySlice<T> = expressionValue1Optional!
    let expressionValue2: ArraySlice<T> = expressionValue2Optional!
    
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotEqual<T : Equatable>(@autoclosure expression1: () -> [T], @autoclosure _ expression2: () -> [T], _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: [T]?
  var expressionValue2Optional: [T]?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: [T] = expressionValue1Optional!
    let expressionValue2: [T] = expressionValue2Optional!
    
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertNotEqual<T, U : Equatable>(@autoclosure expression1: () -> [T: U], @autoclosure _ expression2: () -> [T: U], _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: [T: U]?
  var expressionValue2Optional: [T: U]?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1: [T: U] = expressionValue1Optional!
    let expressionValue2: [T: U] = expressionValue2Optional!
    
    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

func _XCTCheckEqualWithAccuracy_Double(value1: Double, _ value2: Double, _ accuracy: Double) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_Float(value1: Float, _ value2: Float, _ accuracy: Float) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_CGFloat(value1: CGFloat, _ value2: CGFloat, _ accuracy: CGFloat) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

public func XCTAssertEqualWithAccuracy<T : FloatingPointType>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.EqualWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
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
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      let accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

func _XCTCheckNotEqualWithAccuracy_Double(value1: Double, _ value2: Double, _ accuracy: Double) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_Float(value1: Float, _ value2: Float, _ accuracy: Float) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_CGFloat(value1: CGFloat, _ value2: CGFloat, _ accuracy: CGFloat) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

public func XCTAssertNotEqualWithAccuracy<T : FloatingPointType>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ accuracy: T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.NotEqualWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
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
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      let accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertGreaterThan<T : Comparable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.GreaterThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }

  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 > expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    let expressionValueStr1 = "\(expressionValue1)"
    let expressionValueStr2 = "\(expressionValue2)"
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
  }
  
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertGreaterThanOrEqual<T : Comparable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.GreaterThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 >= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertLessThan<T : Comparable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.LessThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 < expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message, file, line)
    }
    
  case .FailedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message, file, line)
    
  case .FailedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message, file, line)
  }
}

public func XCTAssertLessThanOrEqual<T : Comparable>(@autoclosure expression1: () -> T, @autoclosure _ expression2: () -> T, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__)
{
  let assertionType = _XCTAssertionType.LessThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = expression1()
    expressionValue2Optional = expression2()
  }
  
  switch result {
  case .Success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 <= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
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

public func XCTAssertThrows(@autoclosure expression: () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_Throws
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecific(@autoclosure expression: () -> Any?, _ exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecificNamed(@autoclosure expression: () -> Any?, _ exception: Any, _ name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_ThrowsSpecificNamed
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrow(@autoclosure expression: () -> Any?, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrow
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecific(@autoclosure expression: () -> Any?, _ exception: Any, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecificNamed(@autoclosure expression: () -> Any?, _ exception: Any, _ name: String, _ message: String = "", file: String = __FILE__, line: UInt = __LINE__) -> Void {
  let assertionType = _XCTAssertionType.Assertion_NoThrowSpecificNamed
  
  // FIXME: Unsupported
}
#endif
