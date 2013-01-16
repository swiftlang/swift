// RUN: %swift %s -verify

struct X {
  destructor // FIXME: Terrible location info AND this shouldn't be allowed either
} // expected-error {{expected '{' for destructor}}

class Y {
  destructor // FIXME: Terrible location info
} // expected-error {{expected '{' for destructor}}

destructor {} // expected-error{{'destructor' functions may only be declared within a type}}

func foo() {
  destructor {} // expected-error{{'destructor' functions may only be declared within a type}}
}
