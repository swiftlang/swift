// RUN: %target-typecheck-verify-swift

enum E {
  case e1
  case e2
}

enum E2 {
  case `init`, `subscript`, foo, bar
}

func foo1(e : E, i : Int) {
  switch e {} // expected-error{{switch must be exhaustive}}
  // expected-note@-1 {{missing case: '.e1'}}
  // expected-note@-2 {{missing case: '.e2'}}
  switch i {} // expected-error{{'switch' statement body must have at least one 'case' or 'default' block; do you want to add a default case?}}{{13-13=default:\n<#code#>\n}}
}

func foo2(e: E2) {
  switch (e) {
  case .init: break
  // expected-error@-1 {{keyword 'init' cannot be used as an identifier here}}
  // expected-note@-2 {{use backticks to escape it}} {{9-13=`init`}}
  case .subscript: break // Ok
  default: break
  }

  switch (e) {
  case .init, .foo: break
  // expected-error@-1 {{keyword 'init' cannot be used as an identifier here}}
  // expected-note@-2 {{use backticks to escape it}} {{9-13=`init`}}
  case .subscript, .bar: break // Ok
  }

  switch (e) {
  case .`init`, .subscript: break // Ok
  case .foo, .bar: break
  }
}
