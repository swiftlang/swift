// RUN: %target-typecheck-verify-swift -verify-syntax-tree
// FIXME: Too many "unknown syntax" errors.

if true {
  try
  // expected-error@-1 {{unknown expression syntax exists in the source}}
} // expected-error {{expected expression}}

if false {
  [.]
  // expected-error@-1 {{expected identifier after '.' expression}}
  // expected-error@-2 {{unknown expression syntax exists in the source}}
}

class { // expected-error {{unknown declaration syntax exists in the source}}
  // expected-error@-1 {{expected identifier in class declaration}}

}

#if swift(<1)
print("Wat")
class { // expected-error {{unknown declaration syntax exists in the source}}

}
#endif
