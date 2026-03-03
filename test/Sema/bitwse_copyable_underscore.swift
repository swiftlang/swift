// RUN: %target-typecheck-verify-swift                       \
// RUN:     -disable-availability-checking

struct S : _BitwiseCopyable {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable}}{{documentation-file=deprecated-declaration}}

func f<T : _BitwiseCopyable>(_ t: T) {} // expected-warning {{'_BitwiseCopyable' is deprecated: Use BitwiseCopyable}}{{documentation-file=deprecated-declaration}}
