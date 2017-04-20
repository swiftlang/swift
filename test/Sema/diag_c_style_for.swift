// FIXME(integers): with the new integer protocols, the compiler does not seem
// to be able to recognize C-style loops and provide fixits.
// <rdar://problem/29937314>
// XFAIL: *

// RUN: %target-typecheck-verify-swift

for var a = 0; a < 10; a += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}
}

for var b = 0; b < 10; b += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}
}

for var c=1;c != 5 ;c += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{12-18= ..< }} {{20-24=}}
}

for var d=100;d<5;d+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-11= in }} {{14-17= ..< }} {{18-22=}}
}

// next three aren't auto-fixable
for var e = 3; e > 4; e+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

for var f = 3; f < 4; f-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

for var i = 6; i > 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{14-14=).reversed()}} {{14-27=}}
}

for var i = 100; i != 0; i-=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{16-16=).reversed()}} {{16-30=}}
}

let start = Int8(4)
let count = Int8(10)
var other = Int8(2)

for ; other<count; other+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// this should be fixable, and keep the type
for (var number : Int8 = start; number < count; number+=1) { // expected-error {{C-style for statement has been removed in Swift 3}} {{6-10=}} {{23-26= in }} {{31-42= ..< }} {{47-57=}}
  print(number)
}

// should produce extra note
for (var m : Int8 = start; m < count; m+=1) { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
  m += 3
}

for var o = 2; o < 888; o += 1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{23-31=}}
}

for var o = 2; o < 888; o += 11 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}

// could theoretically fix this with "..."
for var p = 2; p <= 8; p+=1 { // expected-error {{C-style for statement has been removed in Swift 3}} {{none}}
}
