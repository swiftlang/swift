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

// The protocol for error values that can be thrown.
// TODO: API review
public protocol ErrorType {
  var _domain: String { get }
  var _code: Int { get }
}

#if _runtime(_ObjC)
// Helper functions for the C++ runtime to have easy access to domain and
// code as Objective-C values.
@asmname("swift_stdlib_getErrorDomainNSString")
public func _stdlib_getErrorDomainNSString<T : ErrorType>(x: UnsafePointer<T>)
-> AnyObject {
  return x.memory._domain._bridgeToObjectiveCImpl()
}

@asmname("swift_stdlib_getErrorCode")
public func _stdlib_getErrorCode<T : ErrorType>(x: UnsafePointer<T>) -> Int {
  return x.memory._code
}

// Known function for the compiler to use to coerce `ErrorType` instances to
// `NSError`.
@asmname("swift_bridgeErrorTypeToNSError")
public func _bridgeErrorTypeToNSError(e: ErrorType) -> AnyObject
#endif
