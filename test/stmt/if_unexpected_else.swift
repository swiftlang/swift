// RUN: %target-typecheck-verify-swift

if true else { // expected-cparser-error {{unexpected 'else' immediately following 'if' condition}} expected-astgen-error {{expected '{' in 'if' statement}} expected-astgen-error {{unexpected code in 'if' statement}}
}              // expected-cparser-note@-1 {{remove 'else' to execute the braced block of statements when the condition is true}} expected-astgen-error {{expected '}' to end 'if' statement}}

