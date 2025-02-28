// %target-swift-frontend -no-color-diagnostics -print-diagnostic-groups -diagnostic-documentation-path %S/test-docs/ -typecheck %s -strict-memory-safety

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -no-color-diagnostics -print-diagnostic-groups -diagnostic-documentation-path %S/test-docs/ -serialize-diagnostics-path %t/serialized.dia %s -strict-memory-safety
// RUN: c-index-test -read-diagnostics %t/serialized.dia > %t/serialized.txt 2>&1
// RUN: %FileCheck %s < %t/serialized.txt

@available(*, deprecated, message: "please do not use")
func f() { }

func g() {
  f()
// CHECK: [[@LINE-1]]:3: warning: 'f()' is deprecated: please do not use [DeprecatedDeclaration] [-W{{.*}}deprecated-declaration.md] [DeprecatedDeclaration]
}


@unsafe
func beCareful() { }

func test() {
  beCareful()
// CHECK: [[@LINE-1]]:3: warning: expression uses unsafe constructs but is not marked with 'unsafe' [StrictMemorySafety] [-W{{.*}}strict-memory-safety.md] [StrictMemorySafety]
}
