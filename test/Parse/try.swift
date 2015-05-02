// RUN: %target-parse-verify-swift

// Intentionally has lower precedence than assignments and ?:
infix operator %%%% { associativity none precedence 50 }
func %%%%<T,U>(x:T, y:U) -> Int { return 0 }

// Intentionally has lower precedence between assignments and ?:
infix operator %%% { associativity none precedence 95 }
func %%%<T,U>(x:T, y:U) -> Int { return 1 }

func foo() throws -> Int { return 0 }
func bar() throws -> Int { return 0 }

var x = try foo() + bar()
x = try foo() + bar()
x += try foo() + bar()
x += try foo() %%%% bar() // expected-error {{'try' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}}
x += try foo() %%% bar()
x = foo() + try bar() // expected-error {{'try' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var y = true ? try foo() : try bar() + 0
var z = true ? try foo() : try bar() %%% 0 // expected-error {{'try' following conditional operator does not cover everything to its right}}

var a = try! foo() + bar()
a = try! foo() + bar()
a += try! foo() + bar()
a += try! foo() %%%% bar() // expected-error {{'try!' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}}
a += try! foo() %%% bar()
a = foo() + try! bar() // expected-error {{'try!' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var b = true ? try! foo() : try! bar() + 0
var c = true ? try! foo() : try! bar() %%% 0 // expected-error {{'try!' following conditional operator does not cover everything to its right}}
