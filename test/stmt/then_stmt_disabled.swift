// RUN: %target-typecheck-verify-swift

// Then statements are disabled by default
let x = if .random() {
  print("hello")
  then 0
  // expected-error@-1 {{cannot find 'then' in scope}}
  // expected-cparser-error@-2 {{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-2 {{consecutive statements on a line must be separated by newline or ';'}}
} else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  1
}
