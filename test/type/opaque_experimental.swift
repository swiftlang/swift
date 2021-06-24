// RUN: %target-typecheck-verify-swift -enable-experimental-opaque-return-types -disable-availability-checking

// Tests for experimental extensions to opaque return type support.

func f0() -> <T> () { }
func f1() -> <T, U, V> () { }
func f2() -> <T: Collection, U: SignedInteger> () { }
func f4() async -> <T> () { }

func g0() -> <T> { } // expected-error{{expected type for function result}}
func g1() -> async <T> () { } // expected-error{{'async' may only occur before '->'}}
func g2() -> <T> () async { } // expected-error{{'async' may only occur before '->'}}
