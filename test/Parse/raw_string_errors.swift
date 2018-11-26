// RUN: %target-typecheck-verify-swift

#"\##("invalid")"#
// expected-error@-1{{too many '#' characters in delimited escape}}
// expected-error@-2{{invalid escape sequence in literal}}

####"invalid"###
// expected-error@-1{{unterminated string literal}}

###"invalid"####
// expected-error@-1{{too many '#' characters in closing delimiter}}
// expected-error@-2{{consecutive statements on a line must be separated by ';'}}
// expected-error@-3{{expected expression}}
// expected-warning@-4{{string literal is unused}}
