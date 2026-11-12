// RUN: %target-swift-frontend -typecheck -strict-memory-safety -Ounchecked -disable-access-control %s 2>&1 | %FileCheck %s

// RUN: not %target-swift-frontend -typecheck -strict-memory-safety -warnings-as-errors -Ounchecked %s 2>&1 | %FileCheck -check-prefix=WERROR %s
// RUN: not %target-swift-frontend -typecheck -warnings-as-errors -strict-memory-safety -Ounchecked %s 2>&1 | %FileCheck -check-prefix=WERROR %s

// Promoting the group enables the diagnostics without '-strict-memory-safety'.
// RUN: not %target-swift-frontend -typecheck -Werror StrictMemorySafety %s 2>&1 | %FileCheck -check-prefix=WERROR-GROUP %s

// '-strict-memory-safety' should enable the group without downgrading '-Werror'.
// RUN: not %target-swift-frontend -typecheck -strict-memory-safety -Werror StrictMemorySafety %s 2>&1 | %FileCheck -check-prefix=WERROR-GROUP %s
// RUN: not %target-swift-frontend -typecheck -Werror StrictMemorySafety -strict-memory-safety %s 2>&1 | %FileCheck -check-prefix=WERROR-GROUP %s

// The conflicts are diagnosed exactly when the command line enables the group.
// RUN: %target-swift-frontend -typecheck -Wwarning StrictMemorySafety -Ounchecked %s 2>&1 | %FileCheck -check-prefix=WWARNING %s
// RUN: %target-swift-frontend -typecheck -Ounchecked -disable-access-control %s 2>&1 | %FileCheck --allow-empty -check-prefix=DEFAULT %s

// CHECK: warning: '-Ounchecked' is not memory-safe
// CHECK: warning: '-disable-access-control' is not memory-safe

// WERROR: error: '-Ounchecked' is not memory-safe

// WWARNING: warning: '-Ounchecked' is not memory-safe

// DEFAULT-NOT: is not memory-safe

// WERROR-GROUP: error: expression uses unsafe constructs but is not marked with 'unsafe'

@unsafe func unsafeFunc() { }

func f() {
  unsafeFunc()
}
