//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if os(Windows)
import WinSDK
#endif

/// A class representing a C++ `std::exception_ptr` instance.
///
/// Instances of this class manage the lifetime of a `std::exception_ptr` value,
/// primarily on behalf of instances of `CaughtException`.
///
/// - Bug: In the future once C++ interop is fully defined and available, this
///   type should be replaced with direct uses of `std::exception_ptr`.
fileprivate final class _ExceptionPointer: RawRepresentable {
  public typealias RawValue = UnsafeMutableRawPointer /* std::exception_ptr * */

  public var rawValue: UnsafeMutableRawPointer

  @_silgen_name("_swift_exceptionPointerCopy")
  private static func __copy(_ ep: UnsafeRawPointer) -> UnsafeMutableRawPointer?

  /// Initialize an instance of this class with a pointer to a C++ exception
  /// pointer (that is, a value of type `const std::exception_ptr *`.)
  ///
  /// - Parameters:
  ///   - rawValue: A pointer to an exception pointer. The exception pointer is
  ///     copied and the caller retains ownership of `rawValue`.
  ///
  /// If `rawValue` represents a `nil` exception, this initializer returns `nil`.
  public init?(rawValue: UnsafeRawPointer) {
    guard let ep = Self.__copy(rawValue) else {
      return nil
    }
    self.rawValue = ep
  }

  /// Initialize an instance of this class with a pointer to a C++ exception
  /// pointer (that is, a value of type `const std::exception_ptr *`.)
  ///
  /// - Parameters:
  ///   - rawValue: A pointer to an exception pointer. The exception pointer is
  ///     copied and the caller retains ownership of `rawValue`.
  ///
  /// If `rawValue` represents a `nil` exception, this initializer returns `nil`.
  public convenience init?(rawValue: UnsafeMutableRawPointer) {
    self.init(rawValue: UnsafeRawPointer(rawValue))
  }

  @_silgen_name("_swift_exceptionPointerIsNil")
  private static func __isNil(_ ep: UnsafeRawPointer) -> Bool

  /// Initialize an instance of this class with a mutable pointer to a C++
  /// exception pointer (that is, a value of type `std::exception_ptr *`) and
  /// take ownership of it.
  ///
  /// - Parameters:
  ///   - rawValue: A mutable pointer to an exception pointer. The exception
  ///     pointer is _not_ copied and the resulting `_ExceptionPointer` instance
  ///     takes ownership of it.
  ///
  /// If `rawValue` represents a `nil` exception, this initializer returns `nil`.
  public init?(rawValueNoCopy rawValue: UnsafeMutableRawPointer) {
    if Self.__isNil(rawValue) {
      return nil
    }
    self.rawValue = rawValue
  }

  @_silgen_name("_swift_exceptionPointerDelete")
  private static func __delete(_ ep: UnsafeMutableRawPointer)

  deinit {
    Self.__delete(rawValue)
  }

  // MARK: -

  @_silgen_name("_swift_exceptionPointerCopyCurrent")
  private static func __copyCurrent() -> UnsafeMutableRawPointer?

  /// The exception that is currently caught, if any.
  ///
  /// If the current code is executing within a C++ `catch` or Objective-C
  /// `@catch` clause, the value of this property equals the exception that
  /// has been caught. Otherwise, the value of this property is `nil`.
  ///
  /// Put in terms of C++: if the value returned by `std::current_exception()`
  /// is equal to `nullptr`, then the value of this property is `nil`.
  static var current: Self? {
    return __copyCurrent().flatMap(Self.init(rawValueNoCopy:))
  }
}

// MARK: -

/// An error representing a previously-caught C++ or Objective-C exception.
///
/// - Warning: Swift support for exception handling is experimental. It remains
///     undefined behavior to throw an exception through a Swift stack frame.
@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
public struct CaughtException: Error {
  private let _ep: _ExceptionPointer

  /// Initialize an instance of this type with a preexisting instance of
  /// `_ExceptionPointer`.
  ///
  /// - Parameters:
  ///   - ep: An existing exception pointer object.
  fileprivate init(_ ep: _ExceptionPointer) {
    _ep = ep
  }

  /// Initialize an instance of this type with a pointer to a C++ exception
  /// pointer (that is, a value of type `const std::exception_ptr *`.)
  ///
  /// - Parameters:
  ///   - ep: A pointer to an exception pointer. The exception pointer is copied
  ///     and the caller retains ownership of `ep`.
  ///
  /// If `ep` represents a `nil` exception, this initializer returns `nil`. If
  /// `ep` is not a valid pointer to a C++ exception pointer, the result is
  /// undefined.
  public init?(exceptionPointer ep: UnsafeRawPointer) {
    guard let ep = _ExceptionPointer(rawValue: ep) else {
      return nil
    }
    self.init(ep)
  }

  /// Initialize an instance of this type with a pointer to a C++ exception
  /// pointer (that is, a value of type `const std::exception_ptr *`.)
  ///
  /// - Parameters:
  ///   - ep: A pointer to an exception pointer. The exception pointer is copied
  ///     and the caller retains ownership of `ep`.
  ///
  /// If `ep` represents a `nil` exception, this initializer returns `nil`. If
  /// `ep` is not a valid pointer to a C++ exception pointer, the result is
  /// undefined.
  public init?(exceptionPointer ep: UnsafeMutableRawPointer) {
    self.init(exceptionPointer: UnsafeRawPointer(ep))
  }

  /// The exception that is currently caught, if any.
  ///
  /// If the current code is executing within a C++ `catch` or Objective-C
  /// `@catch` clause, the value of this property equals the exception that
  /// has been caught. Otherwise, the value of this property is `nil`.
  public static var current: Self? {
    return _ExceptionPointer.current.map(Self.init)
  }
}

// MARK: - Exception inspection

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension CaughtException: CustomStringConvertible,
                           CustomDebugStringConvertible {
  @_silgen_name("_swift_exceptionPointerCopyTypeName")
  private static func __copyTypeName(
    _ ep: UnsafeRawPointer
  ) -> UnsafePointer<CChar>?

  private static func __typeName(_ ep: UnsafeRawPointer) -> String? {
    let utf8 = __copyTypeName(ep)
    defer {
      utf8?.deallocate()
    }

    return utf8.flatMap(String.init(validatingUTF8:))
  }

  @_silgen_name("_swift_exceptionPointerCopyDescription")
  private static func __copyDescription(
    _ ep: UnsafeRawPointer,
    debug: Bool
  ) -> UnsafePointer<CChar>?

  private static func __description(
    _ ep: UnsafeRawPointer,
    debug: Bool
  ) -> String? {
    let utf8 = __copyDescription(ep, debug: debug)
    defer {
      utf8?.deallocate()
    }

    return utf8.flatMap(String.init(validatingUTF8:))
  }

  public var description: String {
    return Self.__description(_ep.rawValue, debug: false)
      ?? "An unknown exception occurred."
  }

  public var debugDescription: String {
    let description = Self.__description(_ep.rawValue, debug: true)
    let typeName = Self.__typeName(_ep.rawValue)

    switch (description, typeName) {
    case (.some(let description), .some(let typeName)):
      return "An exception of type \(typeName) occurred: \(description)"
    case (.some(let description), _):
      return "An exception occurred: \(description)"
    case (_, .some(let typeName)):
      return "An exception of type \(typeName) occurred."
    default:
      return "An unknown exception occurred."
    }
  }
}

// MARK: -

@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
extension CaughtException {
#if _runtime(_ObjC)
  @_silgen_name("_swift_exceptionPointerGetThrownObjectiveCObject")
  private static func __getThrownObjectiveCObject(
    _ ep: UnsafeRawPointer
  ) -> Unmanaged<AnyObject>?

  /// The Objective-C object that was thrown, if any.
  ///
  /// If the represented exception is not an Objective-C object, the value of
  /// this property is `nil`.
  ///
  /// - Note: Objective-C allows throwing objects of any class, not just
  ///     instances of `NSException`.
  public var thrownObjectiveCObject: Any? {
    // Note: Swift expects C functions to return at +1 normally but this
    // C function returns at +0, so Unmanaged is used to manually correct
    // the refcounting.
    return Self.__getThrownObjectiveCObject(_ep.rawValue)?
      .takeUnretainedValue()
  }
#endif

#if os(Windows)
  @_silgen_name("_swift_exceptionPointerGetThrownExceptionRecord")
  private static func __getThrownExceptionRecord(
    _ ep: UnsafeRawPointer,
    _ outER: UnsafeMutablePointer<EXCEPTION_RECORD>
  ) -> Bool

  /// The Windows structured exception code that was thrown, if any.
  ///
  /// If the represented exception is not a Windows structured exception, the
  /// value of this property is `nil`.
  public var thrownExceptionRecord: EXCEPTION_RECORD? {
    return withUnsafeTemporaryAllocation(
      of: EXCEPTION_RECORD.self,
      capacity: 1
    ) { er in
      if Self.__getThrownExceptionRecord(_ep.rawValue, er.baseAddress!) {
        return er.baseAddress!.move()
      }
      return nil
    }
  }
#endif
}

// MARK: - Catching exceptions in Swift

/// Create a `CaughtException` instance from the current thrown-and-caught
/// exception.
///
/// - Returns: A pointer to a newly-allocated instance of `CaughtException` that
///   can be assigned to the `swift_error_result`-annotated parameter of a C
///   function that uses the Swift calling convention. If there is no current
///   exception, this function returns `nil`.
///
/// This function is used by the Swift runtime and standard library to convert a
/// caught exception to an `Error`. C++ callers outside the runtime or standard
/// library might also call it with the following signature:
///
/// ```c++
/// namespace swift {
///   extern "C" void *_Nullable swift_caughtException_copyCurrent(void);
/// }
/// ```
///
/// Swift callers should prefer `CaughtException.current`.
///
/// - Warning: Swift support for exception handling is experimental. It remains
///   undefined behavior to throw an exception through a Swift stack frame.
@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
@_cdecl("swift_caughtException_copyCurrent")
@usableFromInline internal func
swift_caughtException_copyCurrent_DoNotCall() -> UnsafeMutableRawPointer? {
  return CaughtException.current.map { exception in
    Unmanaged.passRetained(exception as AnyObject).toOpaque()
  }
}

@_silgen_name("_swift_withUnsafeExceptionHandling")
private func __withUnsafeExceptionHandling(_ body: () throws -> Void) throws

/// Invoke a closure in a context that can catch Objective-C and C++ exceptions.
///
/// - Parameters:
///   - body: A closure to invoke.
///
/// - Returns: Whatever is returned by `body`.
///
/// - Throws: Whatever is thrown by `body`. Any Objective-C exception or C++
///   exception is thrown as an instance of `CaughtException`. Thrown Swift
///   errors are rethrown as-is.
///
/// - Warning: Swift support for exception handling is experimental. It remains
///   undefined behavior to throw an exception through a Swift stack frame.
@available(macOS 9999.0, iOS 9999.0, watchOS 9999.0, tvOS 9999.0, *)
public func withUnsafeExceptionHandling<R>(_ body: () throws -> R) throws -> R {
  return try withUnsafeTemporaryAllocation(of: R.self, capacity: 1) { result in
    try __withUnsafeExceptionHandling {
      result.baseAddress!.initialize(to: try body())
    }

    return result.baseAddress!.move()
  }
}
