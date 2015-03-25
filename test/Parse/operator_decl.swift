// RUN: %target-parse-verify-swift

prefix operator +++ {}
postfix operator +++ {}
infix operator +++ {}
infix operator +++= {
  associativity right
}
infix operator *** {
  precedence 123
}
infix operator --- {
  precedence 123
  associativity left
}
infix operator >>> {
  precedence 123
  associativity right
}
infix operator &&& {
  associativity none
  precedence 123
}


prefix operator // expected-error {{expected operator name in operator declaration}}

;
prefix operator %%+ // expected-error {{expected '{' after operator name in 'operator' declaration}}

prefix operator %%/ {
  + // expected-error {{expected operator attribute identifier in 'operator' declaration body}}
}


prefix operator %%% {
  associativity none // expected-error{{'associativity' is not a valid prefix operator attribute}}
}
postfix operator %%% {
  associativity none // expected-error{{'associativity' is not a valid postfix operator attribute}}
}

infix operator !!! {
  associativity none
  associativity left // expected-error{{'associativity' for infix operator declared multiple times}}
}

infix operator ^^^ {
  precedence 22 
  precedence 44 // expected-error{{'precedence' for infix operator declared multiple times}}
}

infix operator === {
  associativity free // expected-error{{'free' is not a valid infix operator associativity}}
}

infix operator !== {
  associativity 123 // expected-error{{expected identifier after 'associativity' in 'operator' declaration body}}
}

infix operator !!= {
  precedence blah // expected-error{{expected integer literal after 'precedence' in 'operator' declaration body}}
}

infix operator !<> {
  runcibility 12 // expected-error{{'runcibility' is not a valid infix operator attribute}}
}

class Foo {
  infix operator ||| {} // expected-error{{'operator' may only be declared at file scope}}
}


// rdar://14690497
infix operator ~> { precedence 99999 }   // expected-error {{'precedence' must be in the range of 0 to 255}}

infix operator ->= {
  mutating
}

infix operator ->== {
  mutating mutating // expected-error{{'mutating' for infix operator declared multiple}}
}

infix operator ->=== {
  assignment // expected-error{{'assignment' is no longer a valid infix operator attribute; use 'mutating'}}
}
