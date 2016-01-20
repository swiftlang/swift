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

// The protocol for error values that can be thrown.
// TODO: API review
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
public func _stdlib_getErrorDomainNSString<T : ErrorProtocol>(x: UnsafePointer<T>)
-> AnyObject {
  return x.pointee._domain._bridgeToObjectiveCImpl()
}

@warn_unused_result
@_silgen_name("swift_stdlib_getErrorCode")
public func _stdlib_getErrorCode<T : ErrorProtocol>(x: UnsafePointer<T>) -> Int {
  return x.pointee._code
}

// Known function for the compiler to use to coerce `ErrorProtocol` instances
// to `NSError`.
@warn_unused_result
@_silgen_name("swift_bridgeErrorProtocolToNSError")
public func _bridgeErrorProtocolToNSError(error: ErrorProtocol) -> AnyObject
#endif

/// Invoked by the compiler when the subexpression of a `try!` expression
/// throws an error.
@_silgen_name("swift_unexpectedError")
public func _unexpectedError(error: ErrorProtocol) {
  requirementFailure("'try!' expression unexpectedly raised an error: \(String(reflecting: error))")
}

/// Invoked by the compiler when code at top level throws an uncaught error.
@_silgen_name("swift_errorInMain")
public func _errorInMain(error: ErrorProtocol) {
  fatalError("Error raised at top level: \(String(reflecting: error))")
}

@available(*, unavailable, renamed="ErrorProtocol")
public typealias ErrorType = ErrorProtocol
