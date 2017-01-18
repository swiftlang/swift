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
import SwiftShims

// TODO: API review
/// A type representing an error value that can be thrown.
///
/// Any type that declares conformance to the `Error` protocol can be used to
/// represent an error in Swift's error handling system. Because the `Error`
/// protocol has no requirements of its own, you can declare conformance on
/// any custom type you create.
///
/// Using Enumerations as Errors
/// ============================
///
/// Swift's enumerations are well suited to represent simple errors. Create an
/// enumeration that conforms to the `Error` protocol with a case for each
/// possible error. If there are additional details about the error that could
/// be helpful for recovery, use associated values to include that
/// information.
///
/// The following example shows an `IntParsingError` enumeration that captures
/// two different kinds of errors that can occur when parsing an integer from
/// a string: overflow, where the value represented by the string is too large
/// for the integer data type, and invalid input, where nonnumeric characters
/// are found within the input.
///
///     enum IntParsingError: Error {
///         case overflow
///         case invalidInput(String)
///     }
///
/// The `invalidInput` case includes the invalid character as an associated
/// value.
///
/// The next code sample shows a possible extension to the `Int` type that
/// parses the integer value of a `String` instance, throwing an error when
/// there is a problem during parsing.
///
///     extension Int {
///         init(validating input: String) throws {
///             // ...
///             if !_isValid(s) {
///                 throw IntParsingError.invalidInput(s)
///             }
///             // ...
///         }
///     }
///
/// When calling the new `Int` initializer within a `do` statement, you can use
/// pattern matching to match specific cases of your custom error type and
/// access their associated values, as in the example below.
///
///     do {
///         let price = try Int(validating: "$100")
///     } catch IntParsingError.invalidInput(let invalid) {
///         print("Invalid character: '\(invalid)'")
///     } catch IntParsingError.overflow {
///         print("Overflow error")
///     } catch {
///         print("Other error")
///     }
///     // Prints "Invalid character: '$'"
///
/// Including More Data in Errors
/// =============================
///
/// Sometimes you may want different error states to include the same common
/// data, such as the position in a file or some of your application's state.
/// When you do, use a structure to represent errors. The following example
/// uses a structure to represent an error when parsing an XML document,
/// including the line and column numbers where the error occurred:
///
///     struct XMLParsingError: Error {
///         enum ErrorKind {
///             case invalidCharacter
///             case mismatchedTag
///             case internalError
///         }
///
///         let line: Int
///         let column: Int
///         let kind: ErrorKind
///     }
///
///     func parse(_ source: String) throws -> XMLDoc {
///         // ...
///         throw XMLParsingError(line: 19, column: 5, kind: .mismatchedTag)
///         // ...
///     }
///
/// Once again, use pattern matching to conditionally catch errors. Here's how
/// you can catch any `XMLParsingError` errors thrown by the `parse(_:)`
/// function:
///
///     do {
///         let xmlDoc = try parse(myXMLData)
///     } catch let e as XMLParsingError {
///         print("Parsing error: \(e.kind) [\(e.line):\(e.column)]")
///     } catch {
///         print("Other error: \(error)")
///     }
///     // Prints "Parsing error: mismatchedTag [19:5]"
public protocol Error {
  var _domain: String { get }
  var _code: Int { get }

  // Note: _userInfo is always an NSDictionary, but we cannot use that type here
  // because the standard library cannot depend on Foundation. However, the
  // underscore implies that we control all implementations of this requirement.
  var _userInfo: AnyObject? { get }

#if _runtime(_ObjC)
  func _getEmbeddedNSError() -> AnyObject?
#endif
}

#if _runtime(_ObjC)
extension Error {
  /// Default implementation: there is no embedded NSError.
  public func _getEmbeddedNSError() -> AnyObject? { return nil }
}
#endif

#if _runtime(_ObjC)
// Helper functions for the C++ runtime to have easy access to embedded error,
// domain, code, and userInfo as Objective-C values.
@_silgen_name("swift_stdlib_getErrorDomainNSString")
public func _stdlib_getErrorDomainNSString<T : Error>(_ x: UnsafePointer<T>)
-> AnyObject {
  return x.pointee._domain._bridgeToObjectiveCImpl()
}

@_silgen_name("swift_stdlib_getErrorCode")
public func _stdlib_getErrorCode<T : Error>(_ x: UnsafePointer<T>) -> Int {
  return x.pointee._code
}

// Helper functions for the C++ runtime to have easy access to domain and
// code as Objective-C values.
@_silgen_name("swift_stdlib_getErrorUserInfoNSDictionary")
public func _stdlib_getErrorUserInfoNSDictionary<T : Error>(_ x: UnsafePointer<T>)
-> AnyObject? {
  return x.pointee._userInfo.map { $0 as AnyObject }
}

@_silgen_name("swift_stdlib_getErrorEmbeddedNSErrorIndirect")
public func _stdlib_getErrorEmbeddedNSErrorIndirect<T : Error>(
    _ x: UnsafePointer<T>) -> AnyObject? {
  return x.pointee._getEmbeddedNSError()
}

/// FIXME: Quite unfortunate to have both of these.
@_silgen_name("swift_stdlib_getErrorEmbeddedNSError")
public func _stdlib_getErrorEmbeddedNSError<T : Error>(_ x: T)
-> AnyObject? {
  return x._getEmbeddedNSError()
}

@_silgen_name("swift_stdlib_getErrorDefaultUserInfo")
public func _stdlib_getErrorDefaultUserInfo(_ error: Error) -> AnyObject?

// Known function for the compiler to use to coerce `Error` instances
// to `NSError`.
@_silgen_name("swift_bridgeErrorToNSError")
public func _bridgeErrorToNSError(_ error: Error) -> AnyObject
#endif

/// Invoked by the compiler when the subexpression of a `try!` expression
/// throws an error.
@_silgen_name("swift_unexpectedError")
public func _unexpectedError(_ error: Error) {
  preconditionFailure("'try!' expression unexpectedly raised an error: \(String(reflecting: error))")
}

/// Invoked by the compiler when code at top level throws an uncaught error.
@_silgen_name("swift_errorInMain")
public func _errorInMain(_ error: Error) {
  fatalError("Error raised at top level: \(String(reflecting: error))")
}

/// Runtime function to determine the default code for an Error-conforming type.
@_silgen_name("swift_getDefaultErrorCode")
public func _swift_getDefaultErrorCode<T : Error>(_ x: T) -> Int

@available(*, unavailable, renamed: "Error")
public typealias ErrorType = Error

@available(*, unavailable, renamed: "Error")
public typealias ErrorProtocol = Error

extension Error {
  public var _code: Int {
    return _swift_getDefaultErrorCode(self)
  }

  public var _domain: String {
    return String(reflecting: type(of: self))
  }

  public var _userInfo: AnyObject? {
#if _runtime(_ObjC)
    return _stdlib_getErrorDefaultUserInfo(self)
#else
    return nil
#endif
  }
}

extension Error where Self: RawRepresentable, Self.RawValue: SignedInteger {
  // The error code of Error with integral raw values is the raw value.
  public var _code: Int {
    return numericCast(self.rawValue)
  }
}

extension Error where Self: RawRepresentable, Self.RawValue: UnsignedInteger {
  // The error code of Error with integral raw values is the raw value.
  public var _code: Int {
    return numericCast(self.rawValue)
  }
}
