// RUN: %target-typecheck-verify-swift -verify-syntax-tree
// FIXME: Too many "unknown syntax" errors.

if true {
  try
  // expected-error@-1 {{unknown expression syntax exists in the source}}
} // expected-error {{expected expression}}

if false {
  [.]
  // expected-error@-1 {{expected identifier after '.' expression}}
  // expected-error@-2 2 {{unknown expression syntax exists in the source}}
}

class { // expected-error {{unknown declaration syntax exists in the source}}
  // expected-error@-1 {{expected identifier in class declaration}}
  // expected-note@-2 {{did you mean to use a 'do' statement?}}
  // expected-error@-3 {{closure expression is unused}}
  // expected-error@-4 {{top-level statement cannot begin with a closure expression}}

}
