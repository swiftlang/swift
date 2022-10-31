// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/OtherActors.swiftmodule -module-name OtherActors %S/../Concurrency/Inputs/OtherActors.swift -disable-availability-checking

// RUN: not %target-swift-frontend -typecheck -I %t %s 2>&1 | %FileCheck -check-prefix=DEFAULT %s
// RUN: not %target-swift-frontend -typecheck -I %t %s -suppress-remarks 2>&1 | %FileCheck -check-prefix=NOREMARK %s

@preconcurrency import OtherActors
// DEFAULT:    error: cannot find 'xyz' in scope
// DEFAULT:    remark: '@preconcurrency' attribute on module 'OtherActors' is unused
// NORMEARK:    error: cannot find 'xyz' in scope
// NOREMARK-NOT:    remark: '@preconcurrency' attribute on module 'OtherActors' is unused

func bar() {
    xyz
}
