// RUN: %target-typecheck-verify-swift

// expected-error@+2 {{unterminated string literal}} expected-note@+2 {{add an ending quote to string literal}} {{11-11="}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \

// NOTE: DO NOT add a newline at EOF.
// expected-error@+2 {{unterminated string literal}} expected-note@+2 {{add an ending quote to string literal}} {{11-11="}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \