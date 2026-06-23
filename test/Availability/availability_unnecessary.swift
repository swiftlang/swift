// RUN: %target-typecheck-verify-swift -verify -target %target-cpu-apple-macosx12.0
// REQUIRES: OS=macosx

enum MacOSVersions {
  @available(macOS 10, *)
  case macOS_10
  @available(macOS 11, *)
  case macOS_11
  @available(macOS 12, *)
  case macOS_12
  @available(macOS 13, *)
  case macOS_13
}

@available(macOS 11, *)
func annotatedFunc( // expected-note 3 {{enclosing scope here}}
  _ e: MacOSVersions,
) {
  if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}} {{group-name=UselessAvailabilityCheck}}
  if #available(macOS 11, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
  if #available(macOS 12, *) { }
  if #available(macOS 13, *) { }
  if #available(iOS 8.0, *) { }

  if #unavailable(macOS 10) { }

  func localFunc() {
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
  }
  localFunc()

  switch e {
  case .macOS_10:
    if #available(macOS 10, *) { _ = 1 } // FIXME: [availability] Should be diagnosed
  case .macOS_11:
    if #available(macOS 11, *) { _ = 1 } // FIXME: [availability] Should be diagnosed
  case .macOS_12:
    if #available(macOS 12, *) { _ = 1 }
  case .macOS_13:
    if #available(macOS 13, *) { _ = 1 }
  }
}

@available(macOS 11, *)
class AnnotatedClass { // expected-note {{enclosing scope here}}
  func method() {
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}} {{group-name=UselessAvailabilityCheck}}
  }
}

@available(macOS 11, *)
struct AnnotatedStruct { // expected-note {{enclosing scope here}}
  func method() {
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
  }
}

func testIfStmt() {
  // This check is useless because it is below the deployment target,
  // but it shouldn't warn since there's no scope to point to.
  if #available(macOS 11, *) { }

  if #available(macOS 13, *) { // expected-note {{enclosing scope here}}
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
  }

  if #available(macOS 13, *) {
  } else {
    if #available(macOS 12, *) { }
    if #available(macOS 14, *) { }
  }

  if #available(macOS 11, *) { // expected-note 2 {{enclosing scope here}}
  } else {
    // This branch is unreachable because the deployment target is macOS 12.
    // FIXME: [availability] Spurious warnings
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
    if #available(macOS 13, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
  }
}

func testGuardStmt() {
  func gaurdAboveDeployment() {
    guard #available(macOS 13, *) else { // expected-note {{enclosing scope here}}
      if #available(macOS 12, *) { }
      if #available(macOS 14, *) { }
      return
    }
    if #available(macOS 12, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
    if #available(macOS 14, *) { }
  }

  func gaurdBelowDeployment() {
    guard #available(macOS 11, *) else { // expected-note 2 {{enclosing scope here}}
      // This branch is unreachable because the deployment target is macOS 12.
      // FIXME: [availability] Spurious warnings
      if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
      if #available(macOS 12, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
      return
    }
    if #available(macOS 10, *) { } // FIXME: [availability] Should warn
    if #available(macOS 12, *) { }
  }
}

func testWhileStmt() {
  while #available(macOS 13, *) { // expected-note {{enclosing scope here}}
    if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
    break
  }
}

func testSwitchStmt(
  _ e: MacOSVersions,
) {
  switch e {
  case .macOS_10:
    if #available(macOS 10, *) { _ = 1 }
  case .macOS_11:
    if #available(macOS 11, *) { _ = 1 }
  case .macOS_12:
    if #available(macOS 12, *) { _ = 1 }
  case .macOS_13:
    if #available(macOS 13, *) { _ = 1 }
  }
}


@available(macOS 11, *)
@inlinable
public func fragileFunc() { // expected-note {{enclosing scope here}}
  if #available(macOS 10, *) { } // expected-warning {{unnecessary check for 'macOS'; enclosing scope ensures guard will always be true}}
}

public protocol Mystery { }
public struct Secret: Mystery { public init() { } }
public struct SuperSecret: Mystery { public init() { } }

@available(macOS 11, *)
@inlinable
public func fragileFuncWithOpaqueResult() -> some Mystery {
  // Removing an availability check from a fragile function (e.g. @inlinable)
  // with an opaque result type could change the function's ABI, so the
  // diagnostic is suppressed.
  if #available(macOS 10, *) {
    return Secret()
  }
  return SuperSecret()
}
