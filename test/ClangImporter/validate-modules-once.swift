// RUN: %empty-directory(%t)
// RUN: touch %t/Build.session
// RUN: %swift %s -typecheck -dump-clang-diagnostics -validate-clang-modules-once -clang-build-session-file %t/Build.session 2>&1 | %FileCheck %s

public func foo() {}

// CHECK: '-fmodules-validate-once-per-build-session'
// CHECK: '-fbuild-session-file=
