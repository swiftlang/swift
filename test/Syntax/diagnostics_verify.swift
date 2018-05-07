// RUN: %target-typecheck-verify-swift -verify-syntax-tree
// FIXME: Too many "unknown syntax" errors.

if true {
  try
  // expected-error@-1 2 {{unknown expression syntax exists in the source}}
  // expected-error@-2 {{unknown statement syntax exists in the source}}
} // expected-error {{expected expression}} expected-error {{unknown statement syntax exists in the source}}

if false {
  [.]
  // expected-error@-1 {{expected identifier after '.' expression}}
  // expected-error@-2 2 {{unknown expression syntax exists in the source}}
  // expected-error@-3 {{unknown statement syntax exists in the source}}
} // expected-error {{unknown statement syntax exists in the source}}
