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

// Implementation Note: this file intentionally uses very LOW-LEVEL
// CONSTRUCTS, so that assert and fatal may be used liberally in
// building library abstractions without fear of infinite recursion.
//
// FIXME: We could go farther with this simplification, e.g. avoiding
// UnsafeMutablePointer

@_transparent
public // @testable
func _isDebugAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 0
}

@_transparent
public // @testable, used in _Concurrency executor preconditions
func _isReleaseAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 1
}

@_transparent
public // @testable
func _isFastAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 2
}

@_transparent
public // @testable
func _isStdlibInternalChecksEnabled() -> Bool {
#if INTERNAL_CHECKS_ENABLED
  return true
#else
  return false
#endif
}

@_transparent
@_alwaysEmitIntoClient // Introduced in 5.7
public // @testable
func _isStdlibDebugChecksEnabled() -> Bool {
#if SWIFT_STDLIB_ENABLE_DEBUG_PRECONDITIONS_IN_RELEASE
  return !_isFastAssertConfiguration()
#else
  return _isDebugAssertConfiguration()
#endif
}

@usableFromInline @_transparent
internal func _fatalErrorFlags() -> UInt32 {
  // The current flags are:
  // (1 << 0): Report backtrace on fatal error
#if os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
  return 0
#else
  return _isDebugAssertConfiguration() ? 1 : 0
#endif
}

/// This function should be used only in the implementation of user-level
/// assertions.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
@usableFromInline
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@_semantics("programtermination_point")
internal func _assertionFailure(
  _ prefix: StaticString, _ message: StaticString,
  file: StaticString, line: UInt,
  flags: UInt32
) -> Never {
#if !$Embedded
  prefix.withUTF8Buffer {
    (prefix) -> Void in
    message.withUTF8Buffer {
      (message) -> Void in
      file.withUTF8Buffer {
        (file) -> Void in
        unsafe _swift_stdlib_reportFatalErrorInFile(
          prefix.baseAddress!, CInt(prefix.count),
          message.baseAddress!, CInt(message.count),
          file.baseAddress!, CInt(file.count), UInt32(line),
          flags)
        Builtin.int_trap()
      }
    }
  }
#else
  if _isDebugAssertConfiguration() {
    _embeddedReportFatalErrorInFile(prefix: prefix, message: message,
      file: file, line: line)
  }
#endif
  Builtin.int_trap()
}

/// This function should be used only in the implementation of user-level
/// assertions.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
@usableFromInline
#if !$Embedded
@inline(never)
#else
@_disfavoredOverload
@inline(__always)
#endif
@_semantics("programtermination_point")
internal func _assertionFailure(
  _ prefix: StaticString, _ message: String,
  file: StaticString, line: UInt,
  flags: UInt32
) -> Never {
#if !$Embedded
  prefix.withUTF8Buffer {
    (prefix) -> Void in
    var message = message
    message.withUTF8 {
      (messageUTF8) -> Void in
      file.withUTF8Buffer {
        (file) -> Void in
        unsafe _swift_stdlib_reportFatalErrorInFile(
          prefix.baseAddress!, CInt(prefix.count),
          messageUTF8.baseAddress!, CInt(messageUTF8.count),
          file.baseAddress!, CInt(file.count), UInt32(line),
          flags)
      }
    }
  }
#else
  if _isDebugAssertConfiguration() {
    var message = message
    message.withUTF8 { (messageUTF8) -> Void in
      unsafe _embeddedReportFatalErrorInFile(prefix: prefix, message: messageUTF8, file: file, line: line)
    }
  }
#endif

  Builtin.int_trap()
}

/// This function should be used only in the implementation of user-level
/// assertions.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
@usableFromInline
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@_semantics("programtermination_point")
@_unavailableInEmbedded
internal func _assertionFailure(
  _ prefix: StaticString, _ message: String,
  flags: UInt32
) -> Never {
  prefix.withUTF8Buffer {
    (prefix) -> Void in
    var message = message
    message.withUTF8 {
      (messageUTF8) -> Void in
      unsafe _swift_stdlib_reportFatalError(
        prefix.baseAddress!, CInt(prefix.count),
        messageUTF8.baseAddress!, CInt(messageUTF8.count),
        flags)
    }
  }

  Builtin.int_trap()
}

#if $Embedded
@usableFromInline
@inline(never)
@_semantics("programtermination_point")
internal func _assertionFailure(
  _ prefix: StaticString, _ message: StaticString,
  flags: UInt32
) -> Never {
  if _isDebugAssertConfiguration() {
    _embeddedReportFatalError(prefix: prefix, message: message)
  }

  Builtin.int_trap()
}
#endif

/// This function should be used only in the implementation of stdlib
/// assertions.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
@usableFromInline
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@_semantics("programtermination_point")
internal func _fatalErrorMessage(
  _ prefix: StaticString, _ message: StaticString,
  file: StaticString, line: UInt,
  flags: UInt32
) -> Never {
  _assertionFailure(prefix, message, file: file, line: line, flags: flags)
}

/// Prints a fatal error message when an unimplemented initializer gets
/// called by the Objective-C runtime.
@_transparent
public // COMPILER_INTRINSIC
func _unimplementedInitializer(className: StaticString,
                               initName: StaticString = #function,
                               file: StaticString = #file,
                               line: UInt = #line,
                               column: UInt = #column
) -> Never {
  // This function is marked @_transparent so that it is inlined into the caller
  // (the initializer stub), and, depending on the build configuration,
  // redundant parameter values (#file etc.) are eliminated, and don't leak
  // information about the user's source.

#if !$Embedded
  if _isDebugAssertConfiguration() {
    className.withUTF8Buffer {
      (className) in
      initName.withUTF8Buffer {
        (initName) in
        file.withUTF8Buffer {
          (file) in
          unsafe _swift_stdlib_reportUnimplementedInitializerInFile(
            className.baseAddress!, CInt(className.count),
            initName.baseAddress!, CInt(initName.count),
            file.baseAddress!, CInt(file.count),
            UInt32(line), UInt32(column),
            /*flags:*/ 0)
        }
      }
    }
  } else {
    className.withUTF8Buffer {
      (className) in
      initName.withUTF8Buffer {
        (initName) in
        unsafe _swift_stdlib_reportUnimplementedInitializer(
          className.baseAddress!, CInt(className.count),
          initName.baseAddress!, CInt(initName.count),
          /*flags:*/ 0)
      }
    }
  }
#endif

  Builtin.int_trap()
}

#if !$Embedded

/// Used to evaluate editor placeholders.
public // COMPILER_INTRINSIC
func _undefined<T>(
  _ message: @autoclosure () -> String = String(),
  file: StaticString = #file, line: UInt = #line
) -> T {
  _assertionFailure("Fatal error", message(), file: file, line: line, flags: 0)
}

#else

/// Used to evaluate editor placeholders.
public // COMPILER_INTRINSIC
func _undefined<T>(
  _ message: @autoclosure () -> StaticString = StaticString(),
  file: StaticString = #file, line: UInt = #line
) -> T {
  _assertionFailure("Fatal error", message(), file: file, line: line, flags: 0)
}

#endif

/// Called when falling off the end of a switch and the type can be represented
/// as a raw value.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
///
/// It doesn't take a source location because it's most important
/// in release builds anyway (old apps that are run on new OSs).
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@usableFromInline // COMPILER_INTRINSIC
internal func _diagnoseUnexpectedEnumCaseValue<SwitchedValue, RawValue>(
  type: SwitchedValue.Type,
  rawValue: RawValue
) -> Never {
#if !$Embedded
  _assertionFailure("Fatal error",
                    "unexpected enum case '\(type)(rawValue: \(rawValue))'",
                    flags: _fatalErrorFlags())
#else
  Builtin.int_trap()
#endif
}

/// Called when falling off the end of a switch and the value is not safe to
/// print.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
///
/// It doesn't take a source location because it's most important
/// in release builds anyway (old apps that are run on new OSs).
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@usableFromInline // COMPILER_INTRINSIC
internal func _diagnoseUnexpectedEnumCase<SwitchedValue>(
  type: SwitchedValue.Type
) -> Never {
#if !$Embedded
  _assertionFailure(
    "Fatal error",
    "unexpected enum case while switching on value of type '\(type)'",
    flags: _fatalErrorFlags())
#else
  Builtin.int_trap()
#endif
}

/// Called when a function marked `unavailable` with `@available` is invoked
/// and the module containing the unavailable function was compiled with
/// `-unavailable-decl-optimization=stub`.
///
/// This function should not be inlined in desktop Swift because it is cold and
/// inlining just bloats code. In Embedded Swift, we force inlining as this
/// function is typically just a trap (in release configurations).
@backDeployed(before: SwiftStdlib 5.9)
#if !$Embedded
@inline(never)
#else
@inline(__always)
#endif
@_semantics("unavailable_code_reached")
@usableFromInline // COMPILER_INTRINSIC
internal func _diagnoseUnavailableCodeReached() -> Never {
  _assertionFailure(
    "Fatal error", "Unavailable code reached", flags: _fatalErrorFlags())
}
