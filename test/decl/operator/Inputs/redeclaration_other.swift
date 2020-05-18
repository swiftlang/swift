precedencegroup RedeclaredAcrossFiles {} // expected-note {{previous precedence group declaration here}}

infix operator ^^^ // expected-note {{previous operator declaration here}}
prefix operator >>> // expected-note {{previous operator declaration here}}
postfix operator <<< // expected-note {{previous operator declaration here}}

precedencegroup P1 {}
infix operator ^^^^ : P1 // expected-note {{previous operator declaration here}}

infix operator &&&
