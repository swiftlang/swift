// RUN: %swift %s -verify

func f0() {
  import swift; // expected-error{{import is only valid at file scope}}
}
