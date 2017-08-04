// RUN: %target-typecheck-verify-swift -typecheck %s

for var i = 0; i < 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-27=}}
// expected-error @-2 {{unary operator '++' cannot be applied}}
// expected-note @-3 {{overloads for '++' exist}}

for var i = 0; i < 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-20= ..< }} {{22-30=}}

for var i = 0; i <= 10; i++ {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}}  {{5-9=}} {{10-13= in }} {{14-21= ... }} {{23-28=}}
// expected-error @-2 {{unary operator '++' cannot be applied}}
// expected-note @-3 {{overloads for '++' exist}}

for var i = 0; i <= 10; i += 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{14-21= ... }} {{23-31=}}

for var i = 10; i > 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{15-15=).reversed()}} {{15-27=}}
// expected-error @-2 {{unary operator '--' cannot be applied}}
// expected-note @-3 {{overloads for '--' exist}}

for var i = 10; i > 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=((0 + 1)...}} {{15-15=).reversed()}} {{15-30=}}

for var i = 10; i >= 0; i-- {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=(0...}} {{15-15=).reversed()}} {{15-28=}}
// expected-error @-2 {{unary operator '--' cannot be applied}}
// expected-note @-3 {{overloads for '--' exist}}

for var i = 10; i >= 0; i -= 1 {}
// expected-error @-1 {{C-style for statement has been removed in Swift 3}} {{5-9=}} {{10-13= in }} {{13-13=(0...}} {{15-15=).reversed()}} {{15-31=}}
