// RUN: %target-typecheck-verify-swift

// Make sure we can recover by ignoring the '}' in the #if.
struct S {
  func foo() {
#if true
  } // expected-cparser-error {{unexpected '}' in conditional compilation block}} expected-astgen-error {{unexpected brace in conditional compilation clause}}
#endif
  }
  func bar() {
#if false
  } // expected-cparser-error {{unexpected '}' in conditional compilation block}} expected-astgen-error {{unexpected brace in conditional compilation clause}}
#endif
  }
  func baz() {}
}
