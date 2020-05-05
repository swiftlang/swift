// RUN: %target-typecheck-verify-swift -swift-version 4

#if false || true && false
undefinedIf()
#else
undefinedElse() // expected-error {{cannot find 'undefinedElse' in scope}}
#endif

#if false && true || true
undefinedIf() // expected-error {{cannot find 'undefinedIf' in scope}}
#else
undefinedElse()
#endif

#if false || true && false || false
undefinedIf()
#else
undefinedElse() // expected-error {{cannot find 'undefinedElse' in scope}}
#endif

// expected-error @+1 {{invalid conditional compilation expression}}
#if false || true && try! Swift
#endif
