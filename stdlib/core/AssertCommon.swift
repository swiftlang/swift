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

// Implementation Note: this file intentionally uses very LOW-LEVEL
// CONSTRUCTS, so that assert and fatal may be used liberally in
// building library abstractions without fear of infinite recursion.
//
// FIXME: We could go farther with this simplification, e.g. avoiding
// UnsafeMutablePointer

@transparent
public // @testable
func _isDebugAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 0
}

@transparent
internal func _isReleaseAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 1
}

@transparent
public // @testable
func _isFastAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0: Debug
  // 1: Release
  // 2: Fast
  return Int32(Builtin.assert_configuration()) == 2
}

@transparent
public // @testable
func _isStdlibInternalChecksEnabled() -> Bool {
#if INTERNAL_CHECKS_ENABLED
  return true
#else
  return false
#endif
}

@asmname("swift_reportFatalErrorInFile")
func _reportFatalErrorInFile(
  prefix: UnsafePointer<UInt8>, prefixLength: UWord,
  message: UnsafePointer<UInt8>, messageLength: UWord,
  file: UnsafePointer<UInt8>, fileLength: UWord,
  line: UWord)

@asmname("swift_reportFatalError")
func _reportFatalError(
  prefix: UnsafePointer<UInt8>, prefixLength: UWord,
  message: UnsafePointer<UInt8>, messageLength: UWord)

@asmname("swift_reportUnimplementedInitializerInFile")
func _reportUnimplementedInitializerInFile(
  className: UnsafePointer<UInt8>, classNameLength: UWord,
  initName: UnsafePointer<UInt8>, initNameLength: UWord,
  file: UnsafePointer<UInt8>, fileLength: UWord,
  line: UWord, column: UWord)

@asmname("swift_reportUnimplementedInitializer")
func _reportUnimplementedInitializer(
  className: UnsafePointer<UInt8>, classNameLength: UWord,
  initName: UnsafePointer<UInt8>, initNameLength: UWord)

/// This function should be used only in the implementation of user-level
/// assertions.
///
/// This function should not be inlined because it is cold and it inlining just
/// bloats code.
@noreturn @inline(never)
func _assertionFailed(
  prefix: StaticString, message: StaticString,
  file: StaticString, line: UWord
) {
  prefix.withUTF8Buffer {
    (prefix) -> () in
    message.withUTF8Buffer {
      (message) -> () in
      file.withUTF8Buffer {
        (file) -> () in
        _reportFatalErrorInFile(
          prefix.baseAddress, UWord(prefix.count),
          message.baseAddress, UWord(message.count),
          file.baseAddress, UWord(file.count), line)
        Builtin.int_trap()
      }
    }
  }
  Builtin.int_trap()
}

/// This function should be used only in the implementation of user-level
/// assertions.
///
/// This function should not be inlined because it is cold and it inlining just
/// bloats code.
@noreturn @inline(never)
func _assertionFailed(
  prefix: StaticString, message: String,
  file: StaticString, line: UWord
) {
  prefix.withUTF8Buffer {
    (prefix) -> () in
    let messageUTF8 = message.nulTerminatedUTF8
    messageUTF8.withUnsafeBufferPointer {
      (messageUTF8) -> () in
      file.withUTF8Buffer {
        (file) -> () in
        _reportFatalErrorInFile(
          prefix.baseAddress, UWord(prefix.count),
          messageUTF8.baseAddress, UWord(messageUTF8.count),
          file.baseAddress, UWord(file.count), line)
      }
    }
  }

  Builtin.int_trap()
}

/// This function should be used only in the implementation of stdlib
/// assertions.
///
/// This function should not be inlined because it is cold and it inlining just
/// bloats code.
@noreturn @inline(never)
@semantics("stdlib.noimport")
func _fatalErrorMessage(prefix: StaticString, message: StaticString,
                        file: StaticString, line: UWord) {
#if INTERNAL_CHECKS_ENABLED
  prefix.withUTF8Buffer {
    (prefix) in
    message.withUTF8Buffer {
      (message) in
      file.withUTF8Buffer {
        (file) in
        _reportFatalErrorInFile(
          prefix.baseAddress, UWord(prefix.count),
          message.baseAddress, UWord(message.count),
          file.baseAddress, UWord(file.count), line)
      }
    }
  }
#else
  prefix.withUTF8Buffer {
    (prefix) in
    message.withUTF8Buffer {
      (message) in
      _reportFatalError(
        prefix.baseAddress, UWord(prefix.count),
        message.baseAddress, UWord(message.count))
    }
  }
#endif

  Builtin.int_trap()
}

/// Prints a fatal error message when a unimplemented initializer gets
/// called by the Objective-C runtime.
@transparent @noreturn
public // COMPILER_INTRINSIC
func _unimplemented_initializer(className: StaticString,
                                initName: StaticString = __FUNCTION__,
                                file: StaticString = __FILE__,
                                line: UWord = __LINE__,
                                column: UWord = __COLUMN__) {
  // This function is marked @transparent so that it is inlined into the caller
  // (the initializer stub), and, depending on the build configuration,
  // redundant parameter values (__FILE__ etc.) are eliminated, and don't leak
  // information about the user's source.

  if _isDebugAssertConfiguration() {
    className.withUTF8Buffer {
      (className) in
      initName.withUTF8Buffer {
        (initName) in
        file.withUTF8Buffer {
          (file) in
          _reportUnimplementedInitializerInFile(
            className.baseAddress, UWord(className.count),
            initName.baseAddress, UWord(initName.count),
            file.baseAddress, UWord(file.count), line, column)
        }
      }
    }
  } else {
    className.withUTF8Buffer {
      (className) in
      initName.withUTF8Buffer {
        (initName) in
        _reportUnimplementedInitializer(
          className.baseAddress, UWord(className.count),
          initName.baseAddress, UWord(initName.count))
      }
    }
  }

  Builtin.int_trap()
}
