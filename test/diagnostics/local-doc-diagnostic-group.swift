// RUN: %target-swift-frontend -typecheck -Wwarning PerformanceHints %s 2>&1 | %FileCheck %s --check-prefix=CHECK-FILE -check-prefix CHECK
// REQUIRES: swift_swift_parser

func foo() -> [Int] {
    return [1, 2, 3]
}
// CHECK: warning: Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#PerformanceHints::ReturnTypeImplicitCopy]{{$}}
// CHECK: [#PerformanceHints]: <https://docs.swift.org/compiler/documentation/diagnostics/performance-hints>
// CHECK: [#ReturnTypeImplicitCopy]: <file://{{.*}}share{{/|\\}}doc{{/|\\}}swift{{/|\\}}diagnostics{{/|\\}}return-type-implicit-copy.md>
