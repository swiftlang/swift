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
func assert(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFalied("assertion failed", message, file, line)
    }
  }
}
@transparent
func assert<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _assertionFalied("assertion failed", message, file, line)
    }
  }
}

/// A fatal error occurred and program execution should stop in debug mode.  In
/// optimized builds this is a noop.
@transparent
@noreturn
func fatalError(
  message: StaticString,
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebugAssertConfiguration() {
    _assertionFalied("fatal error", message, file, line)
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
func _precondition(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode. In release mode just trap.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  } else if _isReleaseAssertConfiguration() {
    if !_branchHint(condition(), true) {
      Builtin.int_trap()
    }
  }
}
@transparent
func _precondition<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode. In release mode just trap.
  if _isDebugAssertConfiguration() {
    if !_branchHint(condition(), true) {
      _fatalErrorMessage("fatal error", message, file, line)
    }
  } else if _isReleaseAssertConfiguration() {
    if !_branchHint(condition(), true) {
      Builtin.int_trap()
    }
  }
}

@transparent
@noreturn
func _preconditionFailure(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__) {

  _precondition(false, message, file:file, line: line)

  _conditionallyUnreachable()
}

/// If `error` is true, prints an error message in debug mode, traps in release
/// mode, and returns an undefined error otherwise.
/// Otherwise returns `result`.
@transparent
func _overflowChecked<T>(
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
func _debugPrecondition(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
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
func _debugPrecondition<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
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
@noreturn
func _debugPreconditionFailure(
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
func _sanityCheck(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _fatalErrorMessage("fatal error", message, file, line)
  }
#endif
}

@transparent
func _sanityCheck<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _fatalErrorMessage("fatal error", message, file, line)
  }
#endif
}

@transparent
@noreturn
func _fatalError(
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  _sanityCheck(false, message, file: file, line: line)
  _conditionallyUnreachable();
}
