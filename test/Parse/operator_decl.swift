// RUN: %swift -parse -verify %s

operator prefix +++ {}
operator postfix +++ {}
operator infix +++ {}
operator infix +++= {
  associativity right
}
operator infix *** {
  precedence 123
}
operator infix --- {
  precedence 123
  associativity left
}
operator infix >>> {
  precedence 123
  associativity right
}
operator infix &&& {
  associativity none
  precedence 123
}


operator prefix %%% {
  associativity none // expected-error{{'associativity' is not a valid prefix operator attribute}}
}
operator postfix %%% {
  associativity none // expected-error{{'associativity' is not a valid postfix operator attribute}}
}

operator infix !!! {
  associativity none
  associativity left // expected-error{{'associativity' for infix operator declared multiple times}}
}

operator infix ^^^ {
  precedence 22 
  precedence 44 // expected-error{{'precedence' for infix operator declared multiple times}}
}

operator infix === {
  associativity free // expected-error{{'free' is not a valid infix operator associativity}}
}

operator infix !== {
  associativity 123 // expected-error{{expected identifier after 'associativity' in 'operator' declaration body}}
}

operator infix !!= {
  precedence blah // expected-error{{expected integer literal after 'precedence' in 'operator' declaration body}}
}

operator infix !<> {
  runcibility 12 // expected-error{{'runcibility' is not a valid infix operator attribute}}
}

class Foo {
  operator infix ||| {} // expected-error{{'operator' may only be declared at file scope}}
}
