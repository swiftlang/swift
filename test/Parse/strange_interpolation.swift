// RUN: %target-typecheck-verify-swift -swift-version 4.2
// RUN: %target-run-simple-swift -swift-version 4.2 %s | %FileCheck %s

 let x = 1

 print("[\(x)]")
 // CHECK: [1]

 print("[\(foo: x)]")
 // CHECK: [1]
 // expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
 // expected-note@-3{{remove 'foo' label to keep current behavior}} {{12-17=}}

 print("[\(x, x)]")
 // CHECK: [(1, 1)]
 // expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
 // expected-note@-3{{insert parentheses to keep current behavior}} {{12-12=(}} {{16-16=)}}

 print("[\(foo: x, x)]")
 // CHECK: [(foo: 1, 1)]
 // expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
 // expected-note@-3{{insert parentheses to keep current behavior}} {{12-12=(}} {{21-21=)}}

 print("[\(x, foo: x)]")
 // CHECK: [(1, foo: 1)]
 // expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
 // expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{20-20=)}}

 print("[\(foo: x, foo: x)]")
 // CHECK: [(foo: 1, foo: 1)]
 // expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
 // expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{25-25=)}}

 print("[\(describing: x)]")
 // CHECK: [1]
 // expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
 // expected-note@-3{{remove 'describing' label to keep current behavior}} {{12-24=}}

 print("[\(x, radix: x)]")
 // CHECK: [(1, radix: 1)]
 // expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
 // expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{25-25=)}}

 print("[\(stringInterpolationSegment: x)]")
 // CHECK: [1]
 // expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
 // expected-note@-3{{remove 'stringInterpolationSegment' label to keep current behavior}} {{12-40=}}
