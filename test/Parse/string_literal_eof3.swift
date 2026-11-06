// RUN: %target-typecheck-verify-swift

// expected-cparser-error@+4 {{unterminated string literal}}
// expected-astgen-error@+3 {{expected '"' to end string literal}}
// expected-astgen-note@+2 {{to match this opening '"'}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \

// NOTE: DO NOT add a newline at EOF.
// expected-cparser-error@+4 {{unterminated string literal}}
// expected-astgen-error@+3 {{expected '"' to end string literal}}
// expected-astgen-note@+2 {{to match this opening '"'}}
// expected-error@+1 {{invalid escape sequence in literal}}
_ = "foo \