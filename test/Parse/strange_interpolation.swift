// RUN: %target-typecheck-verify-swift -swift-version 4.2

// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4.2 %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test

print("Begin")
// CHECK: Begin

let x = 1

print("[\(x)]")
// CHECK-NEXT: [1]

print("[\(foo: x)]")
// CHECK-NEXT: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-3{{remove 'foo' label to keep current behavior}} {{11-16=}}

print("[\(x, x)]")
// CHECK-NEXT: [(1, 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{15-15=)}}

print("[\(foo: x, x)]")
// CHECK-NEXT: [(foo: 1, 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{20-20=)}}

print("[\(x, foo: x)]")
// CHECK-NEXT: [(1, foo: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{20-20=)}}

print("[\(foo: x, bar: x)]")
// CHECK-NEXT: [(foo: 1, bar: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{25-25=)}}

print("[\(describing: x)]")
// CHECK-NEXT: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-3{{remove 'describing' label to keep current behavior}} {{11-23=}}

print("[\(x, radix: x)]")
// CHECK-NEXT: [(1, radix: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in Swift 5}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{22-22=)}}

print("[\(stringInterpolationSegment: x)]")
// CHECK-NEXT: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-3{{remove 'stringInterpolationSegment' label to keep current behavior}} {{11-39=}}

print("[ \(foo: "[\(bar: x)]") ]")
// CHECK-NEXT: [ [1] ]
// expected-warning@-2{{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-3{{remove 'foo' label to keep current behavior}} {{12-17=}}
// expected-warning@-4{{labeled interpolations will not be ignored in Swift 5}}
// expected-note@-5{{remove 'bar' label to keep current behavior}} {{21-26=}}

print("End")
// CHECK-NEXT: End
