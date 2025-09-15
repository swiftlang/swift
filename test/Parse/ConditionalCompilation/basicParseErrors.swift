// RUN: %target-typecheck-verify-swift -D FOO -D BAZ -swift-version 4

#if FOO == BAZ // expected-error{{expected '&&' or '||' expression}}
var x = 0
#endif

#if ^FOO // expected-error {{expected unary '!' expression}}
var y = 0
#endif

#if foo(BAR) // expected-error {{invalid conditional compilation expression}}
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

#if foo.bar() // expected-error {{invalid conditional compilation expression}}
      .baz()

#endif


// <https://twitter.com/practicalswift/status/829066902869786625>
#if // expected-error {{incomplete condition in conditional compilation directive}}
#if 0 == // expected-error {{incomplete condition in conditional compilation directive}}
#if 0= // expected-error {{incomplete condition in conditional compilation directive}} expected-error {{'=' must have consistent whitespace on both sides}}
class Foo {
  #if // expected-error {{incomplete condition in conditional compilation directive}}
  #if 0 == // expected-error {{incomplete condition in conditional compilation directive}}
  #if 0= // expected-error {{incomplete condition in conditional compilation directive}} expected-error {{'=' must have consistent whitespace on both sides}}
}

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

#if os(ios)
#endif

#if os(uOS)
#endif

#if os(xxxxxxd)
#endif

#if os(bisionos)

#endif

#if arch(arn)
#endif

#if _endian(mid) // expected-warning {{unknown endianness for build configuration '_endian'}}
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

#if foo || bar || nonExistent() // expected-error {{invalid conditional compilation expression}}
#endif

#if FOO = false
// expected-error @-1 {{invalid conditional compilation expression}}
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}
#endif

#if false
#elseif FOO ? true : false
// expected-error @-1 {{invalid conditional compilation expression}}
undefinedFunc() // ignored.
#else
undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}
#endif

/// Invalid platform condition arguments don't invalidate the whole condition.
// expected-warning@+1{{unknown endianness}}
#if !arch(arn) && !os(ystem) && !_endian(ness)
func fn_k() {}
#endif
fn_k()

#if os(cillator) || arch(i3rm)
func undefinedFunc() // ignored.
#endif
undefinedFunc() // expected-error {{cannot find 'undefinedFunc' in scope}}

#if os(simulator)
#endif

#if arch(iOS)
#endif

#if _endian(arm64) // expected-warning {{unknown endianness for build configuration '_endian'}}
#endif

#if targetEnvironment(_ObjC)
#endif

#if os(iOS) || os(simulator)
#endif

#if arch(ios)
#endif

#if FOO
#else if BAR
// expected-error@-1 {{unexpected 'if' keyword following '#else' conditional compilation directive; did you mean '#elseif'?}} {{1-9=#elseif}}
#else
#endif

#if FOO
#else
if true {}
#endif // OK

// rdar://83017601 Make sure we don't crash
#if canImport()
// expected-error@-1 {{'canImport' requires a module name}}
#endif
