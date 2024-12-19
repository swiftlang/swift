/// Check for reliable availability checking in inlinable code even when
/// skipping some function bodies. rdar://82269657

/// Default build mode reading everything
// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -target %target-cpu-apple-macos10.10 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES,TRC-FULL

/// Emit-module-separately mode / for LLDB
// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -target %target-cpu-apple-macos10.10 -experimental-skip-non-inlinable-function-bodies-without-types 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES,TRC-FULL-NOT

/// InstallAPI mode
// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -target %target-cpu-apple-macos10.10 -experimental-skip-non-inlinable-function-bodies 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE,TRC-WITHTYPES-NOT,TRC-FULL-NOT

/// Index build mode
// RUN: %target-swift-frontend -typecheck -dump-availability-scopes %s -target %target-cpu-apple-macos10.10 -experimental-skip-all-function-bodies 2>&1 \
// RUN:    | %FileCheck %s --check-prefixes TRC-API,TRC-INLINABLE-NOT,TRC-WITHTYPES-NOT,TRC-FULL-NOT

// REQUIRES: OS=macosx

@available(macOS 10.12, *)
public func foo() { }
// TRC-API: (root version=10.10
// TRC-API:   (decl version=10.12 decl=foo()

#if canImport(Swift)
  @available(macOS 10.10, *)
  extension String {
    public var computedVariable: String {
      struct SomeTypeToForceCheckingThis {}

      if #available(macOS 10.12, *) {
        foo()
      }

      fatalError()
    }
  }
#endif
// TRC-FULL:  (decl version=10.10 decl=extension.String
// TRC-WITHTYPES:    (condition_following_availability version=10.12
// TRC-WITHTYPES:    (if_then version=10.12
// TRC-WITHTYPES-NOT-NOT:    (condition_following_availability version=10.12
// TRC-WITHTYPES-NOT-NOT:    (if_then version=10.12

struct S {
  fileprivate var actual: [String] = [] {
    didSet {
      if #available(macOS 10.15, *) {
        foo()
      }
    }
  }
}
// TRC-API:  (condition_following_availability version=10.15
// TRC-API:  (if_then version=10.15

@inlinable public func inlinableFunc() {
    if #available(macOS 10.12, *) {
        foo()
    }
}
// TRC-INLINABLE:  (condition_following_availability version=10.12
// TRC-INLINABLE:  (if_then version=10.12
// TRC-INLINABLE-NOT-NOT:  (condition_following_availability version=10.12
// TRC-INLINABLE-NOT-NOT:  (if_then version=10.12

public func funcWithType() {
    struct S {}
    if #available(macOS 10.13, *) {
        foo()
    }
}
// TRC-WITHTYPES:  (condition_following_availability version=10.13
// TRC-WITHTYPES:  (if_then version=10.13
// TRC-WITHTYPES-NOT-NOT:  (condition_following_availability version=10.13
// TRC-WITHTYPES-NOT-NOT:  (if_then version=10.13

public func funcSkippable() {
    if #available(macOS 10.14, *) {
        foo()
    }
}
// TRC-FULL:  (condition_following_availability version=10.14
// TRC-FULL:  (if_then version=10.14
// TRC-FULL-NOT-NOT:  (condition_following_availability version=10.14
// TRC-FULL-NOT-NOT:  (if_then version=10.14
