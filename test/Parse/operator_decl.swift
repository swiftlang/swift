// RUN: %target-parse-verify-swift

prefix operator +++ {} // expected-warning {{operator should no longer be declared with body}}
postfix operator +++ {} // expected-warning {{operator should no longer be declared with body}}
infix operator +++ {} // expected-warning {{operator should no longer be declared with body}}

prefix operator // expected-error {{expected operator name in operator declaration}}

;
prefix operator %%+

infix operator +++=
infix operator *** : A
infix operator --- : ;

precedencegroup { // expected-error {{expected identifier after 'precedencegroup'}}
  associativity: right
}
precedencegroup A {
  associativity right // expected-error {{expected colon after attribute name in precedence group}}
}
precedencegroup B {
  precedence 123 // expected-error {{'precedence' is not a valid precedence group attribute}}
}
precedencegroup C {
  associativity: sinister // expected-error {{expected 'none', 'left', or 'right' after 'associativity'}}
}
precedencegroup D {
  assignment: no // expected-error {{expected 'true' or 'false' after 'assignment'}}
}
precedencegroup E {
  higherThan:
} // expected-error {{expected name of related precedence group after 'higherThan'}}

precedencegroup F {
  higherThan: A, B, C
}


precedencegroup BangBangBang {
  associativity: none
  associativity: left // expected-error{{'associativity' attribute for precedence group declared multiple times}}
}

precedencegroup CaretCaretCaret {
  assignment: true 
  assignment: false // expected-error{{'assignment' attribute for precedence group declared multiple times}}
}

class Foo {
  infix operator ||| // expected-error{{'operator' may only be declared at file scope}}
}
