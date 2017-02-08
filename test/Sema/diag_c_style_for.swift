// RUN: %target-typecheck-verify-swift

// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var a = 0; a < 10; a++ { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}
}

// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var b = 0; b < 10; ++b { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}
}

// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var c=1;c != 5 ;++c { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{12-18= ..< }} {{20-24=}}
}

// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var d=100;d<5;d++ { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{14-17= ..< }} {{18-22=}}
}

// next three aren't auto-fixable
// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var e = 3; e > 4; e++ { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// expected-error @+1 {{'--' is unavailable: it has been removed in Swift 3}}
for var f = 3; f < 4; f-- { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

for var i = 6; i > 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{14-14=).reversed()}} {{14-27=}}
}

for var i = 100; i != 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{16-16=).reversed()}} {{16-30=}}
}

let start = Int8(4)
let count = Int8(10)
var other = Int8(2)

// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for ; other<count; other++ { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// this should be fixable, and keep the type
// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for (var number : Int8 = start; number < count; number++) { // expected-error {{C-style for statement has been removed in Swift 3}} {{6-10=}} {{23-26= in }} {{31-42= ..< }} {{47-57=}}
  print(number)
}

// should produce extra note
// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for (var m : Int8 = start; m < count; ++m) { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
  m += 3
}

for var o = 2; o < 888; o += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{23-31=}}
}

for var o = 2; o < 888; o += 11 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// could theoretically fix this with "..."
// expected-error @+1 {{'++' is unavailable: it has been removed in Swift 3}}
for var p = 2; p <= 8; p++ { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}
