// RUN: %target-typecheck-verify-swift

if true else { // expected-error {{unexpected 'else' immediately following 'if' condition}}
}              // expected-note@-1 {{remove 'else' to execute the braced block of statements when the condition is true}}

