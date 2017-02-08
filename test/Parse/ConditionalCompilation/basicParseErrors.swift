// RUN: %target-typecheck-verify-swift -D FOO -D BAZ -swift-version 4

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

#if os(ios) // expected-warning {{unknown operating system for build configuration 'os'}} expected-note{{did you mean 'iOS'?}} {{8-11=iOS}}
#endif

#if os(uOS) // expected-warning {{unknown operating system for build configuration 'os'}} expected-note{{did you mean 'iOS'?}} {{8-11=iOS}}
#endif

#if os(xxxxxxd) // expected-warning {{unknown operating system for build configuration 'os'}}
// expected-note@-1{{did you mean 'Linux'?}} {{8-15=Linux}}
// expected-note@-2{{did you mean 'FreeBSD'?}} {{8-15=FreeBSD}}
// expected-note@-3{{did you mean 'Android'?}} {{8-15=Android}}
// expected-note@-4{{did you mean 'OSX'?}} {{8-15=OSX}}
#endif

#if arch(leg) // expected-warning {{unknown architecture for build configuration 'arch'}} expected-note{{did you mean 'arm'?}} {{10-13=arm}}
#endif

#if _endian(mid) // expected-warning {{unknown endianness for build configuration '_endian'}} expected-note{{did you mean 'big'?}} {{13-16=big}}
#endif

LABEL: #if true // expected-error {{expected statement}}
func fn_i() {}
#endif
fn_i() // OK

try #if false // expected-error {{expected expression}}
#else
func fn_j() {}
#endif
fn_j() // OK

#if foo || bar || nonExistent() // expected-error {{expected only one argument to platform condition}}
#endif

#if FOO = false
// expected-error @-1 {{invalid conditional compilation expression}}
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#endif

#if false
#elseif FOO ? true : false
// expected-error @-1 {{invalid conditional compilation expression}}
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{use of unresolved identifier 'undefinedFunc'}}
#endif
