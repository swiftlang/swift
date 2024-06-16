// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-ALL
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warning-as-error availability_deprecated %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-GROUP
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warning-as-error deprecated %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-SUPERGROUP
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -no-warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-ALL-NWAE-ALL
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -no-warning-as-error availability_deprecated %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-ALL-NWAE-GROUP
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -no-warning-as-error deprecated %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-ALL-NWAE-SUPERGROUP
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warning-as-error deprecated -no-warning-as-error availability_deprecated %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-SUPERGROUP-NWAE-GROUP


@available(*, deprecated)
func foo() {
}

@available(*, deprecated, renamed: "bar2")
func bar() {
}


// CHECK-WAE-ALL: error: 'foo()' is deprecated
// CHECK-WAE-ALL-NOT: warning: 'foo()' is deprecated
// CHECK-WAE-GROUP: error: 'foo()' is deprecated
// CHECK-WAE-GROUP-NOT: warning: 'foo()' is deprecated
// CHECK-WAE-SUPERGROUP: error: 'foo()' is deprecated
// CHECK-WAE-SUPERGROUP-NOT: warning: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-ALL: warning: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-ALL-NOT: error: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-GROUP: warning: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-GROUP-NOT: error: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-SUPERGROUP: warning: 'foo()' is deprecated
// CHECK-WAE-ALL-NWAE-SUPERGROUP-NOT: error: 'foo()' is deprecated
// CHECK-WAE-SUPERGROUP-NWAE-GROUP: warning: 'foo()' is deprecated
// CHECK-WAE-SUPERGROUP-NWAE-GROUP-NOT: error: 'foo()' is deprecated
foo()


// CHECK-WAE-ALL: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NOT: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-GROUP: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-GROUP-NOT: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-SUPERGROUP: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-SUPERGROUP-NOT: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-ALL: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-ALL-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-GROUP: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-GROUP-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-SUPERGROUP: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-ALL-NWAE-SUPERGROUP-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-SUPERGROUP-NWAE-GROUP: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-SUPERGROUP-NWAE-GROUP-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
bar()
