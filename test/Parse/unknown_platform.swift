// RUN: %target-typecheck-verify-swift

// expected-error@+1{{invalid conditional compilation expression}}
#if hasGreeble(blah)
#endif

// Future compiler, short-circuit right-hand side
#if compiler(>=10.0) && hasGreeble(blah)
#endif

// Current compiler, short-circuit right-hand side
#if compiler(<10.0) || hasGreeble(blah)
#endif

// This compiler, don't short-circuit.
// expected-error@+1{{invalid conditional compilation expression}}
#if compiler(>=5.7) && hasGreeble(blah)
#endif

// This compiler, don't short-circuit.
// expected-error@+1{{invalid conditional compilation expression}}
#if compiler(<5.8) || hasGreeble(blah)
#endif

// Not a "version" check, so don't short-circuit.
// expected-error@+1{{invalid conditional compilation expression}}
#if os(macOS) && hasGreeble(blah)
#endif
