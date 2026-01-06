// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE
// RUN: not %target-swift-frontend -typecheck -diagnostic-style llvm -Werror DeprecatedDeclaration %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WE-GROUP
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -no-warnings-as-errors %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-NWAE
// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -warnings-as-errors -Wwarning DeprecatedDeclaration %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WAE-WW-GROUP

// This test verifies that the warning control flags apply with respect to 
// the order they are specified in the cmd line.
// Naming:
// WAE: -warnings-as-errors
// NWAE: -no-warnings-as-errors
// WE-xxxx: -Werror xxxx
// WW-xxxx: -Wwarning xxxx
// GROUP - refers to a narrower group
// SUPERGROUP - refers to a broader group that includes GROUP


@available(*, deprecated)
func foo() {
}

@available(*, deprecated, renamed: "bar2")
func bar() {
}


// CHECK-WAE: error: 'foo()' is deprecated
// CHECK-WAE-NOT: warning: 'foo()' is deprecated
// CHECK-WE-GROUP: error: 'foo()' is deprecated
// CHECK-WE-GROUP-NOT: warning: 'foo()' is deprecated
// CHECK-WAE-NWAE: warning: 'foo()' is deprecated
// CHECK-WAE-NWAE-NOT: error: 'foo()' is deprecated
// CHECK-WAE-WW-GROUP: warning: 'foo()' is deprecated
// CHECK-WAE-WW-GROUP-NOT: error: 'foo()' is deprecated
foo()


// CHECK-WAE: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-NOT: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WE-GROUP: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WE-GROUP-NOT: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-NWAE: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-NWAE-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-WW-GROUP: warning: 'bar()' is deprecated: renamed to 'bar2'
// CHECK-WAE-WW-GROUP-NOT: error: 'bar()' is deprecated: renamed to 'bar2'
bar()
