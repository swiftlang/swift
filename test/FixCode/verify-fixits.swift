// RUN: cp %s %t
// RUN: not %swift -typecheck -target %target-triple -verify-apply-fixes %t
// RUN: diff %t %s.result

func f1() {
  guard true { return } // expected-error {{...}}

  guard true { return } // expected-error {{expected 'else' after 'guard' condition}} {{none}}
}