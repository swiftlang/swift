// RUN: %swift %s -verify

struct X { // expected-note{{to match this opening}}
  constructor // FIXME: Terrible location info
} // expected-error {{expected '(' for constructor parameters}}
// expected-error{{expected '}' in struct}}