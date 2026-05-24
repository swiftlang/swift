// RUN: not --crash %target-swift-frontend -typecheck -diagnostics-assert-on-group DeprecatedDeclaration %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -diagnostics-assert-on-group DeprecatedDeclaration -DNO_DEPRECATED %s 2>&1
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -diagnostics-assert-on-group UnknownGroup %s 2>&1 | %FileCheck %s --check-prefix=CHECK-UNKNOWN

// Verify that -diagnostics-assert-on-group traps when any diagnostic
// belonging to the given group is emitted, does nothing when no such diagnostic
// is emitted, and reports an error for an unknown group name.

// CHECK: Trapping on diagnostic group
// CHECK-UNKNOWN: unknown warning group: 'UnknownGroup'

@available(*, deprecated)
func deprecated() {}

#if !NO_DEPRECATED
deprecated()
#endif
