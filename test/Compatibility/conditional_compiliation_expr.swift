// RUN: %target-typecheck-verify-swift -D FOO -swift-version 3


#if FOO = false
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#else
undefinedFunc() // ignored.
#endif

#if false

#elseif !FOO ? false : true
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#endif
