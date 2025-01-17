// RUN: %target-typecheck-verify-swift                       \
// RUN:     -disable-availability-checking                   \
// RUN:     -print-diagnostic-groups

struct S : _BitwiseCopyable {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable [DeprecatedDeclaration]}}

func f<T : _BitwiseCopyable>(_ t: T) {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable [DeprecatedDeclaration]}}
