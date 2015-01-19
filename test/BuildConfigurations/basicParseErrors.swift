// RUN: %target-parse-verify-swift -D FOO -D BAZ

#if FOO == BAZ // expected-error{{expected '&&' or '||' expression}}
var x = 0
#endif

#if ^FOO // expected-error {{expected unary '!' expression}}
var y = 0
#endif

#if swift(FOO) // expected-error {{unexpected target configuration expression (expected 'os' or 'arch')}}
var z = 0
#endif

#if FOO || !FOO
func f() {}
#endif ; f() // expected-error {{extra tokens at the end of the build configuration directive}}

#if FOO || !FOO
func g() {}
#else g()  // expected-error {{extra tokens at the end of the build configuration directive}}
#endif


#if FOO || !FOO
func h() {}
#else /* aaa */
#endif /* bbb */

#if foo.bar() 
      .baz() // expected-error {{unexpected target configuration expression argument type}}

#endif

struct S {
  #if FOO
  #else
  #else  // expected-error {{further conditions after #else are unreachable}}
  #endif
  
  #if FOO
  #elseif BAR
  #elseif BAZ
  #else
  #endif
}

#if FOO
#else
#else  // expected-error {{further conditions after #else are unreachable}}
#endif

#if FOO
#elseif BAR
#elseif BAZ
#else
#endif

