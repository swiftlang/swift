// RUN: %target-typecheck-verify-swift

// Then statements are disabled by default
let x = if .random() {
  print("hello")
  then 0
  // expected-error@-1 {{cannot find 'then' in scope}}
  // expected-error@-2 {{consecutive statements on a line must be separated by ';'}}
} else { // expected-error {{non-expression branch of 'if' expression may only end with a 'throw'}}
  1
}
