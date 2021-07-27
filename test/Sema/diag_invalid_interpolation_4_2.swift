// RUN: %target-typecheck-verify-swift -swift-version 4.2

var x = 0
_ = "\(&x)"
// expected-error@-1 {{'&' used with non-inout argument of type 'Int'}}

_ = "\(y: &x)"
// expected-error@-1 {{'&' used with non-inout argument of type 'Int'}}
// expected-warning@-2 {{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-3 {{remove 'y' label to keep current behavior}}

_ = "\(x, y: &x)"
// expected-error@-1 {{use of extraneous '&'}}
// expected-warning@-2 {{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3 {{insert parentheses to keep current behavior}}
