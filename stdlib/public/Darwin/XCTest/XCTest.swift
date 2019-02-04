//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import XCTest // Clang module

import CoreGraphics
import _SwiftXCTestOverlayShims

// --- XCTest API Swiftification ---

extension XCTContext {

  /// Create and run a new activity with provided name and block.
  public class func runActivity<Result>(named name: String, block: (XCTActivity) throws -> Result) rethrows -> Result {
    let context = _XCTContextCurrent()

    if _XCTContextShouldStartActivity(context, XCTActivityTypeUserCreated) {
      return try autoreleasepool {
        let activity = _XCTContextWillStartActivity(context, name, XCTActivityTypeUserCreated)
        defer {
          _XCTContextDidFinishActivity(context, activity)
        }
        return try block(activity)
      }
    } else {
      fatalError("XCTContext.runActivity(named:block:) failed because activities are disallowed in the current configuration.")
    }
  }
}

#if os(macOS)
@available(swift 4.0)
@available(macOS 10.11, *)
extension XCUIElement {
  /// Types a single key from the XCUIKeyboardKey enumeration with the specified modifier flags.
  @nonobjc public func typeKey(_ key: XCUIKeyboardKey, modifierFlags: XCUIElement.KeyModifierFlags) {
    // Call the version of the method defined in XCTest.framework.
    typeKey(key.rawValue, modifierFlags: modifierFlags)
  }
}
#endif

// --- Failure Formatting ---

/// Register the failure, expected or unexpected, of the current test case.
func _XCTRegisterFailure(_ expected: Bool, _ condition: String, _ message: @autoclosure () -> String, _ file: StaticString, _ line: UInt) {
  // Call the real _XCTFailureHandler.
  let test = _XCTCurrentTestCase()
  _XCTPreformattedFailureHandler(test, expected, file.description, Int(line), condition, message())
}

/// Produce a failure description for the given assertion type.
func _XCTFailureDescription(_ assertionType: _XCTAssertionType, _ formatIndex: Int, _ expressionStrings: CVarArg...) -> String {
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

/// The Swift-style result of evaluating a block which may throw an exception.
enum _XCTThrowableBlockResult {
  case success
  case failedWithError(error: Error)
  case failedWithException(className: String, name: String, reason: String)
  case failedWithUnknownException
}

/// Asks some Objective-C code to evaluate a block which may throw an exception or error,
/// and if it does consume the exception and return information about it.
func _XCTRunThrowableBlock(_ block: () throws -> Void) -> _XCTThrowableBlockResult {
  var blockErrorOptional: Error?

  let exceptionResult = _XCTRunThrowableBlockBridge({
    do {
      try block()
    } catch {
      blockErrorOptional = error
    }
  })

  if let blockError = blockErrorOptional {
    return .failedWithError(error: blockError)
  } else if let exceptionResult = exceptionResult {

    if exceptionResult["type"] == "objc" {
      return .failedWithException(
        className: exceptionResult["className"]!,
        name: exceptionResult["name"]!,
        reason: exceptionResult["reason"]!)
    } else {
      return .failedWithUnknownException
    }
  } else {
    return .success
  }
}

// --- Supported Assertions ---

public func XCTFail(_ message: String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.fail
  
  _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, "" as NSString), message, file, line)
}

public func XCTAssertNil(_ expression: @autoclosure () throws -> Any?, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.`nil`
  
  // evaluate the expression exactly once
  var expressionValueOptional: Any?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = try expression()
  }
  
  switch result {
  case .success:
    // test both Optional and value to treat .none and nil as synonymous
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
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertNil failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertNotNil(_ expression: @autoclosure () throws -> Any?, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.notNil
  
  // evaluate the expression exactly once
  var expressionValueOptional: Any?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = try expression()
  }
  
  switch result {
  case .success:
    // test both Optional and value to treat .none and nil as synonymous
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
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertNotNil failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssert(_ expression: @autoclosure () throws -> Bool, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  // XCTAssert is just a cover for XCTAssertTrue.
  XCTAssertTrue(try expression(), message(), file: file, line: line)
}

public func XCTAssertTrue(_ expression: @autoclosure () throws -> Bool, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.`true`
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = try expression()
  }
  
  switch result {
  case .success:
    let expressionValue = expressionValueOptional!
    
    if !expressionValue {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertTrue failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertFalse(_ expression: @autoclosure () throws -> Bool, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.`false`
  
  // evaluate the expression exactly once
  var expressionValueOptional: Bool?
  
  let result = _XCTRunThrowableBlock {
    expressionValueOptional = try expression()
  }
  
  switch result {
  case .success:
    let expressionValue = expressionValueOptional!
    
    if expressionValue {
      // TODO: @auto_string expression
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertFalse failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertEqual<T : Equatable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.equal
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
    let expressionValue1: T = expressionValue1Optional!
    let expressionValue2: T = expressionValue2Optional!

    if expressionValue1 != expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertNotEqual<T : Equatable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.notEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
    let expressionValue1: T = expressionValue1Optional!
    let expressionValue2: T = expressionValue2Optional!

    if expressionValue1 == expressionValue2 {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertNotEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

func _XCTCheckEqualWithAccuracy_Double(_ value1: Double, _ value2: Double, _ accuracy: Double) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_Float(_ value1: Float, _ value2: Float, _ accuracy: Float) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

func _XCTCheckEqualWithAccuracy_CGFloat(_ value1: CGFloat, _ value2: CGFloat, _ accuracy: CGFloat) -> Bool {
  return (!value1.isNaN && !value2.isNaN)
    && (abs(value1 - value2) <= accuracy)
}

public func XCTAssertEqual<T : FloatingPoint>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, accuracy: T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.equalWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
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
      preconditionFailure("Unsupported floating-point type passed to XCTAssertEqual")
    }
    
    if !equalWithAccuracy {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      let accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

@available(*, deprecated, renamed: "XCTAssertEqual(_:_:accuracy:file:line:)")
public func XCTAssertEqualWithAccuracy<T : FloatingPoint>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, accuracy: T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  XCTAssertEqual(try expression1(), try expression2(), accuracy: accuracy, message(), file: file, line: line)
}

func _XCTCheckNotEqualWithAccuracy_Double(_ value1: Double, _ value2: Double, _ accuracy: Double) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_Float(_ value1: Float, _ value2: Float, _ accuracy: Float) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

func _XCTCheckNotEqualWithAccuracy_CGFloat(_ value1: CGFloat, _ value2: CGFloat, _ accuracy: CGFloat) -> Bool {
  return (value1.isNaN || value2.isNaN)
    || (abs(value1 - value2) > accuracy)
}

public func XCTAssertNotEqual<T : FloatingPoint>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, accuracy: T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.notEqualWithAccuracy
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
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
      preconditionFailure("Unsupported floating-point type passed to XCTAssertNotEqual")
    }
    
    if !notEqualWithAccuracy {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      let accuracyStr = "\(accuracy)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString, accuracyStr as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertNotEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

@available(*, deprecated, renamed: "XCTAssertNotEqual(_:_:accuracy:file:line:)")
public func XCTAssertNotEqualWithAccuracy<T : FloatingPoint>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ accuracy: T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
    XCTAssertNotEqual(try expression1(), try expression2(), accuracy: accuracy, message(), file: file, line: line)
}

public func XCTAssertGreaterThan<T : Comparable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.greaterThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }

  switch result {
  case .success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 > expressionValue2) {
    // TODO: @auto_string expression1
    // TODO: @auto_string expression2
    
    let expressionValueStr1 = "\(expressionValue1)"
    let expressionValueStr2 = "\(expressionValue2)"
    
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
  }
  
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertGreaterThan failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertGreaterThanOrEqual<T : Comparable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line)
{
  let assertionType = _XCTAssertionType.greaterThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 >= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertGreaterThanOrEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertLessThan<T : Comparable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.lessThan
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 < expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertLessThan failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertLessThanOrEqual<T : Comparable>(_ expression1: @autoclosure () throws -> T, _ expression2: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line)
{
  let assertionType = _XCTAssertionType.lessThanOrEqual
  
  // evaluate each expression exactly once
  var expressionValue1Optional: T?
  var expressionValue2Optional: T?
  
  let result = _XCTRunThrowableBlock {
    expressionValue1Optional = try expression1()
    expressionValue2Optional = try expression2()
  }
  
  switch result {
  case .success:
    let expressionValue1 = expressionValue1Optional!
    let expressionValue2 = expressionValue2Optional!
    
    if !(expressionValue1 <= expressionValue2) {
      // TODO: @auto_string expression1
      // TODO: @auto_string expression2
      
      let expressionValueStr1 = "\(expressionValue1)"
      let expressionValueStr2 = "\(expressionValue2)"
      
      _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 0, expressionValueStr1 as NSString, expressionValueStr2 as NSString), message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertLessThanOrEqual failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(false, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

public func XCTAssertThrowsError<T>(_ expression: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line, _ errorHandler: (_ error: Error) -> Void = { _ in }) {
  // evaluate expression exactly once
  var caughtErrorOptional: Error?
  
  let result = _XCTRunThrowableBlock {
    do {
      _ = try expression()
    } catch {
      caughtErrorOptional = error
    }
  }
  
  switch result {
  case .success:
    if let caughtError = caughtErrorOptional {
      errorHandler(caughtError)
    } else {
      _XCTRegisterFailure(true, "XCTAssertThrowsError failed: did not throw an error", message(), file, line)
    }
    
  case .failedWithError(let error):
    _XCTRegisterFailure(false, "XCTAssertThrowsError failed: threw error \"\(error)\"", message(), file, line)
    
  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(true, "XCTAssertThrowsError failed: throwing \(reason)", message(), file, line)
    
  case .failedWithUnknownException:
    _XCTRegisterFailure(true, "XCTAssertThrowsError failed: throwing an unknown exception", message(), file, line)
  }
}

public func XCTAssertNoThrow<T>(_ expression: @autoclosure () throws -> T, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.noThrow

  let result = _XCTRunThrowableBlock { _ = try expression() }

  switch result {
  case .success:
    return

  case .failedWithError(let error):
    _XCTRegisterFailure(true, "XCTAssertNoThrow failed: threw error \"\(error)\"", message(), file, line)

  case .failedWithException(_, _, let reason):
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 1, reason as NSString), message(), file, line)

  case .failedWithUnknownException:
    _XCTRegisterFailure(true, _XCTFailureDescription(assertionType, 2), message(), file, line)
  }
}

#if XCTEST_ENABLE_EXCEPTION_ASSERTIONS
// --- Currently-Unsupported Assertions ---

public func XCTAssertThrows(_ expression: @autoclosure () -> Any?, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.assertion_Throws
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecific(_ expression: @autoclosure () -> Any?, _ exception: Any, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.assertion_ThrowsSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertThrowsSpecificNamed(_ expression: @autoclosure () -> Any?, _ exception: Any, _ name: String, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.assertion_ThrowsSpecificNamed
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecific(_ expression: @autoclosure () -> Any?, _ exception: Any, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.assertion_NoThrowSpecific
  
  // FIXME: Unsupported
}

public func XCTAssertNoThrowSpecificNamed(_ expression: @autoclosure () -> Any?, _ exception: Any, _ name: String, _ message: @autoclosure () -> String = "", file: StaticString = #file, line: UInt = #line) {
  let assertionType = _XCTAssertionType.assertion_NoThrowSpecificNamed
  
  // FIXME: Unsupported
}
#endif
