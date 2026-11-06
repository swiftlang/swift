// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

func missingControllingExprInSwitch() {
  switch // expected-astgen-error {{'switch' statement body must have at least one 'case' or 'default' block; add a default case}}

  switch { // expected-error {{expected expression in 'switch' statement}}
  } // expected-cparser-error {{expected '{' after 'switch' subject expression}} expected-astgen-error {{expected '{}' in 'switch' statement}}
  // expected-error@-2 {{'switch' may only be used as expression in return, throw, or as the source of an assignment}}

  switch // expected-error {{expected expression in 'switch' statement}} expected-error {{'switch' statement body must have at least one 'case' or 'default' block}}
  {
  }

  switch { // expected-error {{expected expression in 'switch' statement}}
    case _: return
  }

  switch { // expected-error {{expected expression in 'switch' statement}}
    case Int: return // expected-error {{'is' keyword required to pattern match against type name}} {{10-10=is }} 
    case _: return
  }

  switch { 42 } { // expected-error {{expected expression in 'switch' statement}} expected-error{{all statements inside a switch must be covered by a 'case' or 'default'}} expected-cparser-error{{consecutive statements on a line must be separated by ';'}} {{16-16=;}} expected-error{{closure expression is unused}} expected-note{{did you mean to use a 'do' statement?}} {{17-17=do }} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block}}
    case _: return // expected-cparser-error{{'case' label can only appear inside a 'switch' statement}} expected-astgen-error {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
  }

  switch { 42 }() { // expected-error {{expected expression in 'switch' statement}} expected-error {{all statements inside a switch must be covered by a 'case' or 'default'}} expected-cparser-error 2 {{consecutive statements on a line must be separated by ';'}} expected-error{{closure expression is unused}} expected-note{{did you mean to use a 'do' statement?}} {{19-19=do }} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}}
    case _: return // expected-cparser-error{{'case' label can only appear inside a 'switch' statement}} expected-astgen-error {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
  }
}
