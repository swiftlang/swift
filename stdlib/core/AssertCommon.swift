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
// UnsafePointer

@transparent
func _isDebugAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0 .. Debug
  // 1 .. Release
  // 2 .. Fast
  return Int32(Builtin.assert_configuration()) == 0;
}

@transparent
func _isReleaseAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0 .. Debug
  // 1 .. Release
  // 2 .. Fast
  return Int32(Builtin.assert_configuration()) == 1;
}

@transparent
func _isFastAssertConfiguration() -> Bool {
  // The values for the assert_configuration call are:
  // 0 .. Debug
  // 1 .. Release
  // 2 .. Fast
  return Int32(Builtin.assert_configuration()) == 2;
}

@asmname("swift_reportFatalErrorInFile")
func _reportFatalErrorInFile(
  prefix: Builtin.RawPointer, prefixLength: Builtin.Word,
  message: Builtin.RawPointer, messageLength: Builtin.Word,
  file: Builtin.RawPointer, fileLength: Builtin.Word,
  line: UWord)

@asmname("swift_reportFatalError")
func _reportFatalError(
  prefix: Builtin.RawPointer, prefixLength: Builtin.Word,
  message: Builtin.RawPointer, messageLength: Builtin.Word)

@asmname("swift_reportUnimplementedInitializerInFile")
func _reportUnimplementedInitializerInFile(
  className: Builtin.RawPointer, classNameLength: Builtin.Word,
  initName: Builtin.RawPointer, initNameLength: Builtin.Word,
  file: Builtin.RawPointer, fileLength: Builtin.Word,
  line: UWord, column: UWord)

@asmname("swift_reportUnimplementedInitializer")
func _reportUnimplementedInitializer(
  className: Builtin.RawPointer, classNameLength: Builtin.Word,
  initName: Builtin.RawPointer, initNameLength: Builtin.Word)

/// This function should be used only in the implementation of user-level
/// assertions.
@noreturn
func _assertionFailed(prefix: StaticString, message: StaticString,
                        file: StaticString, line: UWord) {
  _reportFatalErrorInFile(
      prefix.start, prefix.byteSize, message.start, message.byteSize,
      file.start, file.byteSize, line)

  Builtin.int_trap()
}

/// This function should be used only in the implementation of stdlib
/// assertions.
@transparent
@noreturn
func _fatalErrorMessage(prefix: StaticString, message: StaticString,
                        file: StaticString, line: UWord) {
#if INTERNAL_CHECKS_ENABLED
  _reportFatalErrorInFile(
      prefix.start, prefix.byteSize, message.start, message.byteSize,
      file.start, file.byteSize, line)
#else
  _reportFatalError(prefix.start, prefix.byteSize,
                    message.start, message.byteSize)
#endif

  Builtin.int_trap()
}

/// Prints a fatal error message when a unimplemented initializer gets
/// called by the Objective-C runtime.
@transparent
@noreturn
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
    _reportUnimplementedInitializerInFile(
        className.start, className.byteSize,
        initName.start, initName.byteSize,
        file.start, file.byteSize, line, column)
  } else {
    _reportUnimplementedInitializer(
        className.start, className.byteSize,
        initName.start, initName.byteSize)
  }

  Builtin.int_trap()
}
