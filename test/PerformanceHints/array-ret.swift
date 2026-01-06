// RUN: %empty-directory(%t)

// Ensure ignored by-default
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -verify

// Ensure enabled with '-Wwarning'
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning PerformanceHints &> %t/output_warn.txt
// RUN: cat %t/output_warn.txt | %FileCheck %s -check-prefix CHECK-WARN
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning ReturnTypeImplicitCopy &> %t/output_warn.txt
// RUN: cat %t/output_warn.txt | %FileCheck %s -check-prefix CHECK-WARN

// Ensure enabled with '-Werror' for the broad category and downgraded to warning for the subgroup with '-Wwarning'
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Werror PerformanceHints -Wwarning ReturnTypeImplicitCopy &> %t/output_warn.txt
// RUN: cat %t/output_warn.txt | %FileCheck %s -check-prefix CHECK-WARN

// Ensure enabled with '-Wwarning' for the broad category and downgraded to warning for the subgroup with '-Werror'
// RUN: not %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning PerformanceHints -Werror ReturnTypeImplicitCopy &> %t/output_err.txt
// RUN: cat %t/output_err.txt | %FileCheck %s -check-prefix CHECK-ERR

// Ensure escalated with '-Werror'
// RUN: not %target-swift-frontend -typecheck %s -diagnostic-style llvm -Werror ReturnTypeImplicitCopy &> %t/output_err.txt
// RUN: cat %t/output_err.txt | %FileCheck %s -check-prefix CHECK-ERR

// CHECK-ERR: error: Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#ReturnTypeImplicitCopy]
// CHECK-ERR: error: Performance: closure returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#ReturnTypeImplicitCopy]

// CHECK-WARN: warning: Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#ReturnTypeImplicitCopy]
// CHECK-WARN: warning: Performance: closure returns an array, leading to implicit copies. Consider using an 'inout' parameter instead. [#ReturnTypeImplicitCopy]

func foo() -> [Int] {
    return [1,2,3,4,5,6]
}

func bar() -> Int {
    let myCoolArray = { () -> [Int] in
        return [1, 2]
    }
    return myCoolArray().count
}
