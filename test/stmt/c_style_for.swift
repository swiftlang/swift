// RUN: %target-typecheck-verify-swift

for var i = 0; i < 10; i++ {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 0; i < 10; i += 1 {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i++ {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 0; i <= 10; i += 1 {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i-- {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 10; i > 0; i -= 1 {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i-- {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

for var i = 10; i >= 0; i -= 1 {}
// expected-error @-1 {{C-style for statement was removed in Swift 3}}  {{none}}

let start = Int8(4)
let count = Int8(10)
var other = Int8(2)

for ; other<count; other+=1 { // expected-error {{C-style for statement was removed in Swift 3}} {{none}}
}

for (var number : Int8 = start; number < count; number+=1) { // expected-error {{C-style for statement was removed in Swift 3}} {{none}}
  print(number) // expected-error {{cannot find 'number' in scope}}
}

for (var m : Int8 = start; m < count; m+=1) { // expected-error {{C-style for statement was removed in Swift 3}} {{none}}
  m += 3 // expected-error {{cannot find 'm' in scope}}
}
