// RUN: %target-typecheck-verify-swift

for var i = 0; i < 10; i++ {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i < 10; i += 1 {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i++ {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i += 1 {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i-- {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i -= 1 {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i-- {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i -= 1 {}
// expected-cparser-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}
// expected-astgen-error @-2 {{C-style for statement has been removed in Swift 3}}  {{none}}

let start = Int8(4)
let count = Int8(10)
var other = Int8(2)

for ; other<count; other+=1 { // expected-cparser-error {{C-style for statement was removed in Swift 3}} {{none}} expected-astgen-error {{C-style for statement has been removed in Swift 3}} expected-astgen-error {{expected pattern, 'in', and expression in 'for' statement}}
}

for (var number : Int8 = start; number < count; number+=1) { // expected-cparser-error {{C-style for statement was removed in Swift 3}} {{none}} expected-astgen-error {{C-style for statement has been removed in Swift 3}} expected-astgen-note {{to match this opening '('}} expected-astgen-error {{expected ')' to end tuple pattern}}
  print(number) // expected-cparser-error {{cannot find 'number' in scope}}
}

for (var m : Int8 = start; m < count; m+=1) { // expected-cparser-error {{C-style for statement was removed in Swift 3}} {{none}} expected-astgen-error {{C-style for statement has been removed in Swift 3}} expected-astgen-note {{to match this opening '('}} expected-astgen-error {{expected ')' to end tuple pattern}}
  m += 3 // expected-cparser-error {{cannot find 'm' in scope}}
}
