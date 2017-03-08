// RUN: %swiftc_driver_plain -target %target-triple -module-cache-path %t -typecheck -Xfrontend -verify %s

// This test should be updated to match the expected default Swift version
// when swiftc is invoked directly.
// It should /not/ follow the version specified when invoking lit, which means
// it can't use the %swiftc_driver or %target-build-swift substitutions.

#if swift(>=3)
asdf // expected-error {{use of unresolved identifier}}
#else
jkl
#endif

#if swift(>=3.1)
asdf // expected-error {{use of unresolved identifier}}
#else
jkl
#endif

#if swift(>=4)
aoeu
#else
htn // expected-error {{use of unresolved identifier}}
#endif

#if swift(>=4.1)
aoeu
#else
htn // expected-error {{use of unresolved identifier}}
#endif

#if swift(>=5)
aoeu
#else
htn // expected-error {{use of unresolved identifier}}
#endif
