// RUN: %target-typecheck-verify-swift

prefix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} {{20-23=}} expected-astgen-error {{operator should not be declared with body}}
postfix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} {{21-24=}} expected-astgen-error {{operator should not be declared with body}}
infix operator +++ {} // expected-cparser-error {{operator should no longer be declared with body}} {{19-22=}} expected-astgen-error {{operator should not be declared with body}}
infix operator +++* { // expected-cparser-error {{operator should no longer be declared with body; use a precedence group instead}} {{none}} expected-astgen-error {{operator should not be declared with body}}
  associativity right
}
infix operator +++*+ : A { } // expected-cparser-error {{operator should no longer be declared with body}} {{25-29=}} expected-astgen-error {{operator should not be declared with body}}


prefix operator +++** : A { }
// expected-cparser-error@-1 {{only infix operators may declare a precedence}} {{23-27=}} expected-astgen-error@-1 {{only infix operators may declare a precedence}}
// expected-cparser-error@-2 {{operator should no longer be declared with body}} {{26-30=}} expected-astgen-error@-2 {{operator should not be declared with body}}

prefix operator ++*++ : A
// expected-cparser-error@-1 {{only infix operators may declare a precedence}} {{23-26=}} expected-astgen-error@-1 {{only infix operators may declare a precedence}}

postfix operator ++*+* : A { }
// expected-cparser-error@-1 {{only infix operators may declare a precedence}} {{24-28=}} expected-astgen-error@-1 {{only infix operators may declare a precedence}}
// expected-cparser-error@-2 {{operator should no longer be declared with body}} {{27-31=}} expected-astgen-error@-2 {{operator should not be declared with body}}

postfix operator ++**+ : A
// expected-cparser-error@-1 {{only infix operators may declare a precedence}} {{24-27=}} expected-astgen-error@-1 {{only infix operators may declare a precedence}} expected-astgen-error@-1 {{operator must be declared as 'prefix', 'postfix', or 'infix'}}

operator ++*** : A
// expected-error@-1 {{operator must be declared as 'prefix', 'postfix', or 'infix'}} expected-astgen-error@-1 {{only infix operators may declare a precedence}}

operator +*+++ { }
// expected-error@-1 {{operator must be declared as 'prefix', 'postfix', or 'infix'}}
// expected-cparser-error@-2 {{operator should no longer be declared with body}} {{15-19=}} expected-astgen-error@-2 {{operator should not be declared with body}}

operator +*++* : A { }
// expected-cparser-error@-1 {{operator must be declared as 'prefix', 'postfix', or 'infix'}} expected-astgen-error@-1 {{only infix operators may declare a precedence}}
// expected-cparser-error@-2 {{operator should no longer be declared with body}} {{19-23=}} expected-astgen-error@-2 {{operator should not be declared with body}}

prefix operator // expected-cparser-error {{expected operator name in operator declaration}} expected-astgen-error {{expected binary operator in operator declaration}}

;
prefix operator %%+

prefix operator ??
postfix operator ?? // expected-error {{postfix operator names starting with '?' or '!' are disallowed to avoid collisions with built-in unwrapping operators}}
prefix operator !!
postfix operator !! // expected-error {{postfix operator names starting with '?' or '!' are disallowed to avoid collisions with built-in unwrapping operators}}
postfix operator ?$$
// expected-error@-1 {{postfix operator names starting with '?' or '!' are disallowed}}
// expected-error@-2 {{'$$' is considered an identifier}}

infix operator --aa // expected-error {{'aa' is considered an identifier and must not appear within an operator name}}
infix operator aa--: A // expected-error {{'aa' is considered an identifier and must not appear within an operator name}}
infix operator <<$$@< // expected-cparser-error {{'$$' is considered an identifier and must not appear within an operator name}} expected-astgen-error {{'$$@<' is not allowed in operator names}}
infix operator !!@aa // expected-cparser-error {{'@' is not allowed in operator names}} expected-astgen-error {{'@aa' is not allowed in operator names}}
infix operator #++= // expected-error {{'#' is not allowed in operator names}}
infix operator ++=# // expected-error {{'#' is not allowed in operator names}}
infix operator -># // expected-error {{'#' is not allowed in operator names}}

// FIXME: Ideally, we shouldn't emit the «consistent whitespace» diagnostic
// where = cannot possibly mean an assignment.
infix operator =#=
// expected-cparser-error@-1 {{'#' is not allowed in operator names}}
// expected-cparser-error@-2 {{'=' must have consistent whitespace on both sides}} expected-astgen-error@-2 {{'#=' is not allowed in operator names}}

infix operator +++=
infix operator *** : A
infix operator --- : ; // expected-cparser-error {{expected precedence group name after ':' in operator declaration}} expected-astgen-error {{expected precedence group in operator declaration}}

precedencegroup { // expected-cparser-error {{expected identifier after 'precedencegroup'}} expected-astgen-error {{expected identifier in precedencegroup}}
  associativity: right
}
precedencegroup A {
  associativity right // expected-cparser-error {{expected colon after attribute name in precedence group}} expected-astgen-error {{expected ':' in 'associativity' property of precedencegroup}}
}
precedencegroup B {
  precedence 123 // expected-cparser-error {{'precedence' is not a valid precedence group attribute}} expected-astgen-error {{unexpected code 'precedence 123' in precedencegroup}}
}
precedencegroup C {
  associativity: sinister // expected-cparser-error {{expected 'none', 'left', or 'right' after 'associativity'}} expected-astgen-error {{Expected 'none', 'left', or 'right' after 'associativity'}}
}
precedencegroup D {
  assignment: no // expected-error {{expected 'true' or 'false' after 'assignment'}}
}
precedencegroup E {
  higherThan: // expected-astgen-error {{expected name in 'relation' property of precedencegroup}}
} // expected-cparser-error {{expected name of related precedence group after 'higherThan'}}
precedencegroup EE {
  higherThan: E, // expected-astgen-error {{expected name in 'relation' property of precedencegroup}}
} // expected-cparser-error {{expected name of related precedence group after 'higherThan'}}

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

infix operator **<< : UndeclaredPrecedenceGroup
// expected-error@-1 {{unknown precedence group 'UndeclaredPrecedenceGroup'}}

protocol Proto {}
infix operator *<*< : F, Proto
// expected-error @-1 {{consecutive statements on a line must be separated by ';'}}
// expected-error @-2 {{expected expression}}

// https://github.com/apple/swift/issues/60932

// expected-cparser-error@+2 {{expected precedence group name after ':' in operator declaration}}
postfix operator ++: // expected-cparser-error {{only infix operators may declare a precedence}} {{20-21=}} expected-astgen-error {{only infix operators may declare a precedence}} expected-astgen-error {{expected precedence group in operator declaration}}
