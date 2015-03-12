// RUN: %target-parse-verify-swift

// Intentionally has lower precedence than assignments and ?:
infix operator %%%% { associativity none precedence 50 }
func %%%%<T,U>(x:T, y:U) -> Int { return 0 }

// Intentionally has lower precedence between assignments and ?:
infix operator %%% { associativity none precedence 95 }
func %%%<T,U>(x:T, y:U) -> Int { return 1 }

// These can be 'throws' later.
func foo() -> Int { return 0 }
func bar() -> Int { return 0 }

var x = try foo() + bar()
x = try foo() + bar()
x += try foo() + bar()
x += try foo() %%%% bar() // expected-error {{'try' following assignment operator does not cover everything to its right}}
x += try foo() %%% bar()
x = foo() + try bar() // expected-error {{'try' cannot appear to the right of a non-assignment operator}}

var y = true ? try foo() : try bar() + 0
var z = true ? try foo() : try bar() %%% 0 // expected-error {{'try' following conditional operator does not cover everything to its right}}
