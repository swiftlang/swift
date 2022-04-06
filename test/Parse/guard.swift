// RUN: %target-typecheck-verify-swift

func noConditionNoElse() {
  guard {} // expected-error {{missing condition in 'guard' statement}} expected-error {{expected 'else' after 'guard' condition}}
}
func noCondition() {
  guard else {} // expected-error {{missing condition in 'guard' statement}} 
}
