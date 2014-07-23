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
/// User code assertions and fatal errors are only enabled in debug mode. In
/// release or fast mode these checks are disabled. This means they may have no
/// effect on program semantics, depending on the assert configuration.

/// Traditional C-style assert with an optional message.
/// When assertions are enabled and `condition` is false, stop program
/// execution in a debuggable state after printing a message.  When
/// assertions are disabled in release and fast builds, `condition` is not even
/// evaluated.
@transparent
public func assert(
  condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFailed("assertion failed", message, file, line)
    }
  }
}

@transparent
public func assert<T : BooleanType>(
  condition: @autoclosure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFailed("assertion failed", message, file, line)
    }
  }
}

/// A fatal error occurred and program execution should stop in debug mode.  In
/// optimized builds this is a noop.
@transparent @noreturn
public func fatalError(
  message: StaticString,
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebugAssertConfiguration() {
    _assertionFailed("fatal error", message, file, line)
  }
  _conditionallyUnreachable()
}

/// Library precondition checks
///
/// Library precondition checks are enabled in debug mode and release mode. When
/// building in fast mode they are disabled.  In release mode they don't print
/// an error message but just trap. In debug mode they print an error message
/// and abort.
@transparent
public func _precondition(
  condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
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
@transparent
public func _precondition<T : BooleanType>(
  condition: @autoclosure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode. In release mode just trap.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  } else if _isReleaseAssertConfiguration() {
    let error = !condition().boolValue;
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
  condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  }
}

@transparent
public func _debugPrecondition<T : BooleanType>(
  condition: @autoclosure () -> T, _ message: StaticString = StaticString(),
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
  condition: @autoclosure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _fatalErrorMessage("fatal error", message, file, line)
  }
#endif
}

@transparent
public func _sanityCheck<T : BooleanType>(
  condition: @autoclosure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _fatalErrorMessage("fatal error", message, file, line)
  }
#endif
}

@transparent @noreturn
public func _fatalError(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  _sanityCheck(false, message, file: file, line: line)
  _conditionallyUnreachable();
}
