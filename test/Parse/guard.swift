// RUN: %target-typecheck-verify-swift

func noConditionNoElse() {
  guard {} // expected-error {{missing condition in 'guard' statement}} expected-astgen-error {{cannot convert value of type '() -> ()' to expected condition type 'Bool'}} expected-cparser-error {{expected 'else' after 'guard' condition}} expected-astgen-error {{expected 'else' and body in 'guard' statement}}
}
func noCondition() {
  guard else {} // expected-cparser-error {{missing condition in 'guard' statement}} expected-astgen-error {{expected condition in 'guard' statement}} 
}
