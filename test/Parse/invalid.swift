// RUN: %swift -parse -verify %s

func foo(a:Int) {
  foo(<#a#>) // expected-error {{'<' is not a prefix unary operator}} expected-error 2{{invalid character}} expected-error {{'>' is not a postfix unary operator}}
  foo(<@a@>) // expected-error {{'<' is not a prefix unary operator}} expected-error 2{{invalid character}} expected-error {{'>' is not a postfix unary operator}}
}
