//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An error produced when a throwing C++ function raises an exception.
///
/// The message is a copy of the exception's `what()` string, decoded as UTF-8
/// with invalid sequences replaced. Exceptions that do not derive from
/// `std::exception` have the message `"Unknown C++ exception"`.
public struct CxxException: Error, Sendable {
  /// A copy of the C++ exception's diagnostic message.
  public let message: String

  public init(message: String) {
    self.message = message
  }
}

/// Runs a compiler-generated adapter with temporary exception storage.
///
/// The body must invoke its callback synchronously, at most once. Neither the
/// context nor its callback may escape the body. This function is an
/// implementation detail of C++ interoperability.
@_alwaysEmitIntoClient
public func _withCxxExceptionCapture<Result>(
  _ body: (
    UnsafeMutableRawPointer?,
    @convention(c) (UnsafeMutableRawPointer?, UnsafePointer<CChar>?) -> Void
  ) -> Result
) throws -> Result {
  var exception: CxxException?
  let result = withUnsafeMutablePointer(to: &exception) { context in
    unsafe body(UnsafeMutableRawPointer(context), _captureCxxException)
  }
  if let exception { throw exception }
  return result
}

/// Copies a message while the C++ exception that owns it is still alive.
///
/// The compiler passes this function as a C function pointer. `context` must
/// point to initialized `CxxException?` storage. The callback is invoked at most
/// once and does not escape the scope of `_withCxxExceptionCapture`.
@unsafe
@_alwaysEmitIntoClient
public func _captureCxxException(
  _ context: UnsafeMutableRawPointer?,
  _ message: UnsafePointer<CChar>?
) {
  let exception = unsafe context!.assumingMemoryBound(to: CxxException?.self)
  let copiedMessage: String
  if let message = unsafe message {
    copiedMessage = unsafe String(cString: message)
  } else {
    copiedMessage = "Unknown C++ exception"
  }
  unsafe exception.pointee = CxxException(message: copiedMessage)
}
