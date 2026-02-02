// RUN: not %target-swift-frontend -typecheck %s -diagnostic-style llvm \
// RUN:   -define-availability "_brokenParse:a b c d" \
// RUN:   -define-availability ":a b c d" \
// RUN:   -define-availability "_justAName" \
// RUN:   -define-availability "_brokenPlatforms:spaceOS 10.11" \
// RUN:   -define-availability "_refuseWildcard:iOS 13.0, *, macOS 11.0" \
// RUN:   -define-availability "_incorrectCase:ios 13.0, macos 11.0" \
// RUN:   -define-availability "_noVersion: macOS" \
// RUN:   -define-availability "_duplicateVersion 1.0:iOS 13.0" \
// RUN:   -define-availability "_duplicateVersion 1.0:iOS 13.0" \
// RUN:   2>&1 | %FileCheck %s

// Force reading the argument macros.
@available(_brokenPlatforms)
public func brokenPlatforms() {}

@available(_incorrectCase)
public func incorrectCase() {}

@available(_noVersion)
public func noVersion() {}

@available(_noVersionMulti)
public func noVersionMulti() {}

// CHECK: -define-availability argument:1:1: error: expected an identifier to begin an availability macro definition
// CHECK-NEXT: :a b c d

// CHECK: -define-availability argument:1:11: error: expected ':' after '_justAName' in availability macro definition
// CHECK-NEXT: _justAName

// CHECK: -define-availability argument:1:27: error: future platforms identified by '*' cannot be used in an availability macro
// CHECK-NEXT: _refuseWildcard

// CHECK: duplicate definition of availability macro '_duplicateVersion' for version '1.0'
// CHECK-NEXT: _duplicateVersion

// CHECK: -define-availability argument:1:18: warning: unrecognized platform name 'spaceOS'; did you mean 'macOS'?
// CHECK-NEXT: _brokenPlatforms:spaceOS 10.11

// CHECK: -define-availability argument:1:26: warning: unrecognized platform name 'macos'; did you mean 'macOS'?
// CHECK-NEXT: _incorrectCase

// CHECK: -define-availability argument:1:16: warning: unrecognized platform name 'ios'; did you mean 'iOS'?
// CHECK-NEXT: _incorrectCase

// FIXME: [availability] Diagnostic needs improvement
// CHECK: -define-availability argument:1:13: warning: expected 'introduced', 'deprecated', or 'obsoleted' in '@available' attribute for macOS
// CHECK-NEXT: _noVersion: macOS
