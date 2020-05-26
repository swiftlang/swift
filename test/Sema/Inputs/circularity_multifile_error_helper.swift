struct External {
  var member: Something // expected-error {{cannot find type 'Something' in scope}}
}

struct OtherExternal {}
