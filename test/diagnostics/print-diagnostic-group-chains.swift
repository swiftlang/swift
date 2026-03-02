// RUN: %target-swift-frontend -typecheck %s -Wwarning ExistentialType 2>&1 | %FileCheck %s --check-prefix=CHECK
// REQUIRES: swift_swift_parser

@available(*, deprecated, renamed: "bar2")
func bar() {}
// CHECK: warning: 'bar()' is deprecated: renamed to 'bar2' [#DeprecatedDeclaration]
bar()

protocol Animal {}
struct Tiger: Animal {}

// CHECK: warning: {{.*}} returns an existential{{.*}} [#PerformanceHints::ExistentialType]
func getAnimal() -> any Animal { return Tiger() }

// Verify that both parent and child groups appear in footnotes.
// CHECK-DAG: [#DeprecatedDeclaration]: <{{.*}}deprecated-declaration>
// CHECK-DAG: [#ExistentialType]: <{{.*}}existential-type>
// CHECK-DAG: [#PerformanceHints]: <{{.*}}performance-hints>
