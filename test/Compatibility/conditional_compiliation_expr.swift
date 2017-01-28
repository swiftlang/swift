// RUN: %target-typecheck-verify-swift -D FOO -swift-version 3


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
