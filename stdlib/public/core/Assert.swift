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

/// User code assertions.
///
/// User code assertions are only enabled in debug mode. In release and
/// unchecked modes these checks are disabled.  This means they may have no
/// effect on program semantics, depending on the assert configuration.

/// Traditional C-style assert with an optional message.
///
/// When assertions are enabled and `condition` is false, stop program
/// execution in a debuggable state after printing a message.  When
/// assertions are disabled in release and fast builds, `condition` is not even
/// evaluated.
///
/// When assertions are turned off, the optimizer can assume that the
/// `condition` is true.
@transparent
public func assert(
  @autoclosure condition: () -> Bool,
  @autoclosure _ message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFailed("assertion failed", message(), file, line)
    }
  }
}

/// Ensure that the `condition` is true.
///
/// If the `condition` is false, in debug and release modes the program stops.
///
/// In unchecked mode the optimizer can assume that the `condition` is true.
@transparent
public func precondition(
  @autoclosure condition: () -> Bool,
  @autoclosure _ message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode.  In release mode just trap.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFailed("precondition failed", message(), file, line)
    }
  } else if _isReleaseAssertConfiguration() {
    let error = !condition()
    Builtin.condfail(error.value)
  }
}

/// A fatal error occurred and program execution should stop in debug mode.  In
/// optimized builds this is a noop.
@transparent @noreturn
public func assertionFailure(
  @autoclosure _ message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebugAssertConfiguration() {
    _assertionFailed("fatal error", message(), file, line)
  }
  _conditionallyUnreachable()
}

/// A fatal error occurred and program execution should stop in debug mode and
/// in optimized mode.  In unchecked builds this is a noop, but the
/// optimizer can still assume that the call is unreachable.
@transparent @noreturn
public func preconditionFailure(
  @autoclosure _ message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode.  In release mode just trap.
  if _isDebugAssertConfiguration() {
    _assertionFailed("fatal error", message(), file, line)
  } else if _isReleaseAssertConfiguration() {
    Builtin.int_trap()
  }
  _conditionallyUnreachable()
}

/// A fatal error occurred and program execution should stop in debug,
/// optimized and unchecked modes.
@transparent @noreturn
public func fatalError(
  @autoclosure _ message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  _assertionFailed("fatal error", message(), file, line)
}

/// Library precondition checks
///
/// Library precondition checks are enabled in debug mode and release mode. When
/// building in fast mode they are disabled.  In release mode they don't print
/// an error message but just trap. In debug mode they print an error message
/// and abort.
@transparent
public func _precondition(
  @autoclosure condition: () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode. In release mode just trap.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  } else if _isReleaseAssertConfiguration() {
    let error = !condition()
    Builtin.condfail(error.value)
  }
}

@transparent @noreturn
public func _preconditionFailure(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__) {

  _precondition(false, message, file:file, line: line)

  _conditionallyUnreachable()
}

/// If `error` is true, prints an error message in debug mode, traps in release
/// mode, and returns an undefined error otherwise.
/// Otherwise returns `result`.
@transparent
public func _overflowChecked<T>(
  args: (T, Bool),
  file: StaticString = __FILE__, line: UWord = __LINE__
) -> T {
  let (result, error) = args
  if _isDebugAssertConfiguration() {
    if _branchHint(error, false) {
      _fatalErrorMessage("fatal error", "Overflow/underflow", file, line)
    }
  } else {
    Builtin.condfail(error.value)
  }
  return result
}


/// Debug library precondition checks
///
/// Debug library precondition checks are only on in debug mode. In release and
/// in fast mode they are disabled. In debug mode they print an error message
/// and abort.
/// They are meant to be used when the check is not comprehensively checking for
/// all possible errors.
@transparent
public func _debugPrecondition(
  @autoclosure condition: () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  }
}

@transparent @noreturn
public func _debugPreconditionFailure(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__) {
  if _isDebugAssertConfiguration() {
    _precondition(false, message, file: file, line: line)
  }
  _conditionallyUnreachable()
}

/// Internal checks
///
/// Internal checks are to be used for checking correctness conditions in the
/// standard library. They are only enable when the standard library is built
/// with the build configuration INTERNAL_CHECKS_ENABLED enabled. Otherwise, the
/// call to this function is a noop.
@transparent
public func _sanityCheck(
  @autoclosure condition: () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _fatalErrorMessage("fatal error", message, file, line)
  }
#endif
}

@transparent @noreturn
public func _sanityCheckFailure(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  _sanityCheck(false, message, file: file, line: line)
  _conditionallyUnreachable()
}
