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

//
// Fatal errors, which always stop the program regardless of build
// configuration.
//

/// A fatal error occurred and program execution must stop.  In
/// builds with assertions enabled, currently prints message to the
/// console and executes a minimal debugger trap.  In non-assert
/// builds, just executes a minimal debugger trap.
@transparent
@noreturn
func fatal(
  message: StaticString,
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebug() {
    _fatal_error_message("fatal error", message, file, line)
  } else if _isFast() {
    _conditionallyUnreachable()
  } else {
    Builtin.int_trap()
  }
}

/// If condition is false, invoke `fatal(message)`
@transparent
func securityCheck<L: LogicValue>(
  condition: L,
  _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if _isDebug() {
    if !_branchHint(condition.getLogicValue(), true) {
      _fatal_error_message("fatal error", message, file, line)
    }
  } else {
    if !_branchHint(condition.getLogicValue(), true) {
      Builtin.int_trap()
    }
  }
}

/// If `error` is true, executes `fatal("Overflow/underflow")`.
/// Otherwise returns `result`.
@transparent
func overflowChecked<T>(
  args: (T, Bool),
  file: StaticString = __FILE__, line: UWord = __LINE__
) -> T {
  let (result, error) = args
  if _isDebug() {
    if _branchHint(error, false) {
      fatal("Overflow/underflow", file: file, line: line)
    }
  } else {
    Builtin.condfail(error.value)
    return result
  }
  return result
}

//
// Assertions, which may have no effect on program semantics,
// depending on the build configuration
//

// FIXME: Dispatching to this overload is needed as a workaround for
// <rdar://problem/15889294>
@transparent
func assert(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug mode.
  if _isDebug() {
    if !_branchHint(condition(), true) {
      _fatal_error_message("assertion failed", message, file, line)
    }
  }
}

/// Traditional C-style assert with an optional message.
///
/// When assertions are enabled and `condition` is false, stop program
/// execution in a debuggable state after printing a message.  When
/// assertions are disabled, `condition` is not even evaluated.
@transparent
func assert<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only in debug mode.
  if _isDebug() {
    assert(condition().getLogicValue(), message, file: file, line: line)
  }
}

/// Internal checks are to be used for checking correctness conditions in the
/// standard library. They are only enable when the standard library is built
/// with the build configuration INTERNAL_CHECKS_ENABLED enabled. Otherwise, the
/// call to this function is a noop.
@transparent
func _internal_check(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
      _fatal_error_message("internal check failed", message, file, line)
  }
#endif
}

@transparent
func _internal_check<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
#if INTERNAL_CHECKS_ENABLED
  if !_branchHint(condition(), true) {
    _internal_check(condition().getLogicValue(), message, file: file, line: line)
  }
#endif
}

/// Library precondition checks are enabled in debug and release mode but not in
/// fast mode. In release mode they don't print an error message but just trap.
/// In debug mode they print an error message and abort.
@transparent
func _precondition_safety_check(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only check in debug and release mode. In release mode just trap.
  if _isDebug() {
    if !_branchHint(condition(), true) {
      _fatal_error_message("precondition failed", message, file, line)
    }
  } else if _isRelease() {
    if !_branchHint(condition(), true) {
      Builtin.int_trap()
    }
  }
}

@transparent
func _precondition_safety_check<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only in debug and release mode.
  if _isDebug() || _isRelease() {
    _precondition_safety_check(condition().getLogicValue(), message, file: file, line: line)
  }
}

/// Library partial safety checks are only enabled in debug mode and print an
/// error message on failure.
@transparent
func _partial_safety_check(
  condition: @auto_closure () -> Bool, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only assert in debug and release mode.
  if _isDebug() {
    if !_branchHint(condition(), true) {
      _fatal_error_message("safety check failed", message, file, line)
    }
  }
}

@transparent
func _partial_safety_check<T : LogicValue>(
  condition: @auto_closure () -> T, _ message: StaticString = StaticString(),
  file: StaticString = __FILE__, line: UWord = __LINE__
) {
  // Only in debug mode.
  if _isDebug() {
    _partial_safety_check(condition().getLogicValue(), message, file: file, line: line)
  }
}
