// RUN: %target-typecheck-verify-swift -swift-version 4

#if false || true && false
undefinedIf()
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

#if false && true || true
undefinedIf() // expected-error {{use of unresolved identifier 'undefinedIf'}}
#else
undefinedElse()
#endif

#if false || true && false || false
undefinedIf()
#else
undefinedElse() // expected-error {{use of unresolved identifier 'undefinedElse'}}
#endif

// expected-error @+1 {{invalid conditional compilation expression}}
#if false || true && try! Swift
#endif
