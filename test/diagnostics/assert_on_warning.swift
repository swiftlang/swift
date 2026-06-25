// RUN: not --crash %target-swift-frontend -typecheck -diagnostics-assert-on-warning %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -diagnostics-assert-on-warning -DNO_WARNING %s 2>&1

// Verify that -diagnostics-assert-on-warning traps when a warning is
// emitted and does nothing when no warnings are emitted.

// CHECK: We emitted a warning?!

@available(*, deprecated)
func deprecated() {}

#if !NO_WARNING
deprecated()
#endif
