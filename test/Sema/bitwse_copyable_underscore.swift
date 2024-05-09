// RUN: %target-typecheck-verify-swift                       \
// RUN:     -disable-availability-checking                   \
// RUN:     -debug-diagnostic-names

struct S : _BitwiseCopyable {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable}}

func f<T : _BitwiseCopyable>(_ t: T) {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable}}
