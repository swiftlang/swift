// RUN: %target-typecheck-verify-swift

#"\##("invalid")"#
// expected-error@-1{{Too many # characters in delimited escape}}

####"invalid"###
// expected-error@-1{{unterminated string literal}}

###"invalid"####
// expected-error@-1{{unterminated string literal}}
