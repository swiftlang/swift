// RUN: %swift -parse %s -verify

func f(x : @inout Int) { } // okay

func g() -> @inout Int { } // expected-error{{'inout' attribute can only be applied to function parameters}}

func h(_ : @inout Int) -> (@inout Int) -> (@inout Int)
  -> @inout Int { } // expected-error{{'inout' attribute can only be applied to function parameters}}

func ff(x: (@inout Int, @inout Float)) { } // FIXME: <rdar://problem/15456130> makes this easy to detect

struct X<T> { }

var i : @inout Int // expected-error{{'inout' attribute can only be applied to function parameters}}


enum inout_carrier {
  case carry(@inout Int) // expected-error {{'inout' attribute can only be applied to function parameters}}
}
