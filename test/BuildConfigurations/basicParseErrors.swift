// RUN: %swift -parse %s -verify -D FOO -D BAZ
// REQUIRES: X86

#if FOO == BAZ // expected-error{{expected '&&' or '||' expression}}
var x = 0
#endif

#if ^FOO // expected-error{{expected unary '!' expression}}
var y = 0
#endif

#if swift(FOO) // expected-error{{unexpected target configuration expression (expected 'os' or 'arch')}}
var z = 0
#endif

