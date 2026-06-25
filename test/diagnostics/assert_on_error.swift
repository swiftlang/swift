// RUN: not --crash %target-swift-frontend -typecheck -diagnostics-assert-on-error %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -diagnostics-assert-on-error -DNO_ERROR %s 2>&1

// Verify that -diagnostics-assert-on-error traps when an error is emitted
// and does nothing when no errors are emitted.

// CHECK: We emitted an error?!

#if !NO_ERROR
let _: Int = "not an int"
#endif
