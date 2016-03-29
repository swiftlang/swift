//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// TODO: API review
/// A type representing an error value that can be thrown.
///
/// Any type that declares conformance to `ErrorProtocol` can be used to
/// represent an error in Swift's error handling system. Because
/// `ErrorProtocol` has no requirements of its own, you can declare
/// conformance on any custom type you create.
///
/// Using Enumerations as Errors
/// ============================
///
/// Swift's enumerations are well suited to represent simple errors. Create an
/// enumeration that conforms to `ErrorProtocol` with a case for each possible
/// error. If there are additional details about the error that could be
/// helpful for recovery, use associated values to include that information.
///
/// The following example shows an `IntParsingError` enumeration that captures
/// two different kinds of errors that can occur when parsing an integer from
/// a string: overflow, where the value represented by the string is too large
/// for the integer data type, and invalid input, where nonnumeric characters
/// are found within the input.
///
///     enum IntParsingError: ErrorProtocol {
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
///     struct XMLParsingError: ErrorProtocol {
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
public protocol ErrorProtocol {
  var _domain: String { get }
  var _code: Int { get }
}

extension ErrorProtocol {
  public var _domain: String {
    return String(reflecting: self.dynamicType)
  }
}

#if _runtime(_ObjC)
// Helper functions for the C++ runtime to have easy access to domain and
// code as Objective-C values.
@warn_unused_result
@_silgen_name("swift_stdlib_getErrorDomainNSString")
public func _stdlib_getErrorDomainNSString<T : ErrorProtocol>(_ x: UnsafePointer<T>)
-> AnyObject {
  return x.pointee._domain._bridgeToObjectiveCImpl()
}

@warn_unused_result
@_silgen_name("swift_stdlib_getErrorCode")
public func _stdlib_getErrorCode<T : ErrorProtocol>(_ x: UnsafePointer<T>) -> Int {
  return x.pointee._code
}

// Known function for the compiler to use to coerce `ErrorProtocol` instances
// to `NSError`.
@warn_unused_result
@_silgen_name("swift_bridgeErrorProtocolToNSError")
public func _bridgeErrorProtocolToNSError(_ error: ErrorProtocol) -> AnyObject
#endif

/// Invoked by the compiler when the subexpression of a `try!` expression
/// throws an error.
@_silgen_name("swift_unexpectedError")
public func _unexpectedError(_ error: ErrorProtocol) {
  preconditionFailure("'try!' expression unexpectedly raised an error: \(String(reflecting: error))")
}

/// Invoked by the compiler when code at top level throws an uncaught error.
@_silgen_name("swift_errorInMain")
public func _errorInMain(_ error: ErrorProtocol) {
  fatalError("Error raised at top level: \(String(reflecting: error))")
}

@available(*, unavailable, renamed: "ErrorProtocol")
public typealias ErrorType = ErrorProtocol
