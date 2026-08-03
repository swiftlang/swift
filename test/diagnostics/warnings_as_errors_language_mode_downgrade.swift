// RUN: %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s --check-prefix=NOWAE
// RUN: %target-swift-frontend -typecheck -warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=WAE
// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/90933
// Language-mode downgrade wrappers must not claim a "future" error when the
// diagnostic is upgraded to an error (e.g. via -warnings-as-errors).

typealias F = () -> ()

func foo(_ f: @escaping @Sendable F) {}

// NOWAE: warning: attribute '@Sendable' cannot be applied to a type alias; this will be an error in a future Swift language mode
// WAE: error: attribute '@Sendable' cannot be applied to a type alias
// WAE-NOT: this will be an error
