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

/// Traditional C-style assert with an optional message.
///
/// Use this function for internal sanity checks that are active
/// during testing but do not impact performance of shipping code.
/// To check for invalid usage in Release builds; see `precondition`.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration): if `condition` evaluates to false, stop program
///   execution in a debuggable state after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration),
///   `condition` is not evaluated, and there are no effects.
///
/// * In -Ounchecked builds, `condition` is not evaluated, but the
///   optimizer may assume that it *would* evaluate to `true`. Failure
///   to satisfy that assumption in -Ounchecked builds is a serious
///   programming error.
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

/// Check a necessary condition for making forward progress.
///
/// Use this function to detect conditions that must prevent the
/// program from proceeding even in shipping code.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration): if `condition` evaluates to false, stop program
///   execution in a debuggable state after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration):
///   if `condition` evaluates to false, stop program execution.
///
/// * In -Ounchecked builds, `condition` is not evaluated, but the
///   optimizer may assume that it *would* evaluate to `true`. Failure
///   to satisfy that assumption in -Ounchecked builds is a serious
///   programming error.
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

/// Indicate that an internal sanity check failed.
///
/// Use this function to stop the program, without impacting the
/// performance of shipping code, when control flow is not expected to
/// reach the call (e.g. in the `default` case of a `switch` where you
/// have knowledge that one of the other cases must be satisfied). To
/// protect code from invalid usage in Release builds; see
/// `preconditionFailure`.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration) stop program execution in a debuggable state
///   after printing `message`.
///
/// * In -O builds, has no effect.
///
/// * In -Ounchecked builds, the optimizer may assume that this
///   function will never be called. Failure to satisfy that assumption
///   is a serious programming error.
@inline(__always)
public func assertionFailure(
  @autoclosure message: () -> String = String(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebugAssertConfiguration() {
    _assertionFailed("fatal error", message(), file, line)
  }
  else if _isFastAssertConfiguration() {
    _conditionallyUnreachable()
  }
}

/// Indicate that a precondition was violated.
///
/// Use this function to stop the program when control flow can only
/// reach the call if your API was improperly used.
///
/// * In playgrounds and -Onone builds (the default for Xcode's Debug
///   configuration), stop program execution in a debuggable state
///   after printing `message`.
///
/// * In -O builds (the default for Xcode's Release configuration),
///   stop program execution.
///
/// * In -Ounchecked builds, the optimizer may assume that this
///   function will never be called. Failure to satisfy that assumption
///   is a serious programming error.
@transparent @noreturn
public func preconditionFailure(
  @autoclosure message: () -> String = String(),
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

/// Unconditionally print a `message` and stop execution.
@transparent @noreturn
public func fatalError(
  @autoclosure message: () -> String = String(),
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
  message: StaticString = StaticString(),
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
  message: StaticString = StaticString(),
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
  message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  _sanityCheck(false, message, file: file, line: line)
  _conditionallyUnreachable()
}
