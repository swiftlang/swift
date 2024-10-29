// RUN: not %target-swift-frontend -typecheck -diagnostic-style=llvm -exit-on-first-error %s 2>&1 | %FileCheck %s

#warning("a warning") // CHECK: warning: a warning

#error("an error") // CHECK: error: an error

// We exit after the first error, so never encounter this.
#error("another error") // CHECK-NOT: another error
