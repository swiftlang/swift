// RUN: %target-typecheck-verify-swift -typecheck %s

for var i = 0; i < 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i < 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{none}}
