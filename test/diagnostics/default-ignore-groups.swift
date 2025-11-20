// RUN: %empty-directory(%t)

// Ignore `ReturnTypeImplicitCopy` warnings by-default, because its parent group (`PerformanceHints`) is marked `DefaultIgnoreWarnings`
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -verify

// Ensure enabled with `-Wwarning`
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning ReturnTypeImplicitCopy -verify -verify-additional-prefix warnonly-

// Ensure enabled with `-Wwarning` on the parent group
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Wwarning PerformanceHints -verify -verify-additional-prefix warnonly-

// Ensure enabled with `-Werror`
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Werror ReturnTypeImplicitCopy -verify -verify-additional-prefix erronly-

// Ensure enabled with `-Werror` on the parent group
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -Werror PerformanceHints -verify -verify-additional-prefix erronly-

// Ensure enabling sibling group does not enable this group
// RUN: %target-swift-frontend -typecheck %s -diagnostic-style llvm -verify -Werror ExistentialType


func foo() -> [Int] {
// expected-erronly-error@-1 {{Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead}}
// expected-warnonly-warning@-2 {{Performance: 'foo()' returns an array, leading to implicit copies. Consider using an 'inout' parameter instead}}    
    return [1,2,3,4,5,6]
}
