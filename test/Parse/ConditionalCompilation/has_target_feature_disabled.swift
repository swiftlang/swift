// RUN: %target-typecheck-verify-swift

#if _hasTargetFeature("avx2") // expected-error {{invalid conditional compilation expression}}
#warning("has avx2")
#else
#warning("no avx2") // expected-warning {{no avx2}}
#endif
