// RUN: %swiftc_driver_plain -parse -Xfrontend -verify %s

// This test should be updated to match the expected default Swift version
// when swiftc is invoked directly.
// It should /not/ follow the version specified when invoking lit.

#if swift(>=3)
asdf // expected-error {{use of unresolved identifier}}
#else
jkl
#endif

#if swift(>=4)
aoeu
#else
htn // expected-error {{use of unresolved identifier}}
#endif
