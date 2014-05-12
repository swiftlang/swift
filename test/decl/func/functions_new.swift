// RUN: %swift %s -verify

func test_recovery_missing_name_1(var: Int) {} // expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}

func test_recovery_missing_name_2(let: Int) {} // expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}


// <rdar://problem/16792027> compiler infinite loops on a really really mutating function
struct F {
  mutating mutating mutating f() { // expected-error 2 {{'mutating' specified twice}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error 2 {{expected declaration}}
  }
}
