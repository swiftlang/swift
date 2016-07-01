// RUN: %target-parse-verify-swift -D FOO -D BAZ

#if FOO == BAZ // expected-error{{expected '&&' or '||' expression}}
var x = 0
#endif

#if ^FOO // expected-error {{expected unary '!' expression}}
var y = 0
#endif

#if foo(BAR) // expected-error {{unexpected platform condition (expected 'os', 'arch', or 'swift')}}
var z = 0
#endif

#if FOO || !FOO
func f() {}
#endif ; f() // expected-error {{extra tokens following conditional compilation directive}}

#if FOO || !FOO
func g() {}
#else g()  // expected-error {{extra tokens following conditional compilation directive}}
#endif


#if FOO || !FOO
func h() {}
#else /* aaa */
#endif /* bbb */

#if foo.bar() 
      .baz() // expected-error {{unexpected platform condition (expected 'os', 'arch', or 'swift')}}

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

#if os(youOS) // expected-warning {{unknown operating system for build configuration 'os'}}
#endif

#if arch(leg) // expected-warning {{unknown architecture for build configuration 'arch'}}
#endif

#if _endian(mid) // expected-warning {{unknown endianness for build configuration '_endian'}}
#endif
