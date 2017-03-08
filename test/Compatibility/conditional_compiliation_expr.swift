// RUN: %target-typecheck-verify-swift -D FOO -swift-version 3

// ---------------------------------------------------------------------------
// Invalid binary operation

#if FOO = false
// expected-warning @-1 {{ignoring invalid conditional compilation expression, which will be rejected in future version of Swift}}
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#else
undefinedFunc() // ignored.
#endif

#if false

#elseif !FOO ? false : true
// expected-warning @-1 {{ignoring invalid conditional compilation expression, which will be rejected in future version of Swift}}
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#endif

// ---------------------------------------------------------------------------
// SR-3663: The precedence and associativity of '||' and '&&'.

#if false || true && false
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
undefinedElse()
#endif

#if false && true || true
undefinedIf()
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

#if false || true && false || false
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
undefinedElse()
#endif

// Accepted in Swift3.
#if false || true && try! Swift
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}} 
#endif

// ---------------------------------------------------------------------------
// SR-4032: "skip parsing" in non-active branch for version checks.

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
foo bar baz // expected-error 2 {{consecutive statements}}
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
foo bar baz // expected-error 2 {{consecutive statements}}
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

#if FOO && swift(>=2.2)
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
// There should be no error here.
foo bar baz // expected-error 2 {{consecutive statements}}
#endif

#if swift(>=2.2) && swift(>=1)
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
// There should be no error here.
foo bar baz // expected-error 2 {{consecutive statements}}
#endif

// ---------------------------------------------------------------------------
// SR-4031: Compound name in compilation condition

#if BAR(_:)
#elseif os(x:)(macOS)
#elseif os(Linux(foo:bar:))
#endif
