// RUN: %swift %s -verify

func test_recovery_missing_name_1(var: Int) {} // expected-error {{type annotation missing in pattern}} expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}

func test_recovery_missing_name_2(let: Int) {} // expected-error {{type annotation missing in pattern}} expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}
