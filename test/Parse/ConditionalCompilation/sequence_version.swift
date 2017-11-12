// RUN: %target-typecheck-verify-swift -swift-version 4 -D FOO

#if !swift(>=2.2)
// There should be no error here.
foo bar
#else
let _: Int = 1
#endif

#if (swift(>=2.2))
let _: Int = 1
#else
// There should be no error here.
foo bar
#endif

#if swift(>=99.0) || swift(>=88.1.1)
// There should be no error here.
foo bar baz
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

#if swift(>=99.0) || FOO
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
undefinedElse()
#endif

#if swift(>=99.0) && FOO
// There should be no error here.
foo bar baz
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

#if FOO && swift(>=2.2)
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
// There should be no error here.
foo bar baz
#endif

#if swift(>=2.2) && swift(>=1)
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
// There should be no error here.
foo bar baz
#endif
