// RUN: %target-swift-frontend -typecheck -diagnostic-style llvm -print-diagnostic-groups %s 2>&1 | %FileCheck %s --check-prefix=CHECK


// This test checks that "-print-diagnostic-groups" prints the diagnostic group
// if it exists, and prints nothing if it does not.


@available(*, deprecated, renamed: "bar2")
func bar() {
}

// CHECK: warning: 'bar()' is deprecated: renamed to 'bar2' [DeprecatedDeclaration]{{$}}
bar()


func foo() {
  // CHECK: warning: initialization of immutable value 'x' was never used; consider replacing with assignment to '_' or removing it{{$}}
  let x = 42
}