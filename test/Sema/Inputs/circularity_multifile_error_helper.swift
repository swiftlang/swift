struct External {
  var member: Something // expected-error {{use of undeclared type 'Something'}}
}

struct OtherExternal {}
