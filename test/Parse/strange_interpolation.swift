// RUN: %target-run-simple-swift -verify %s | %FileCheck %s

let x = 1

print("[\(x)]")
// CHECK: [1]

print("[\(foo: x)]")
// CHECK: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in future versions of Swift}}
// expected-note@-3{{remove 'foo' label to keep current behavior}} {{11-16=}}

print("[\(x, x)]")
// CHECK: [(1, 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in future versions of Swift}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{15-15=)}}

print("[\(foo: x, x)]")
// CHECK: [(foo: 1, 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in future versions of Swift}}
// expected-note@-3{{insert parentheses to keep current behavior}} {{11-11=(}} {{20-20=)}}

print("[\(x, foo: x)]")
// CHECK: [(1, foo: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in future versions of Swift}}
// expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{20-20=)}}

print("[\(foo: x, foo: x)]")
// CHECK: [(foo: 1, foo: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in future versions of Swift}}
// expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{25-25=)}}

print("[\(describing: x)]")
// CHECK: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in future versions of Swift}}
// expected-note@-3{{remove 'describing' label to keep current behavior}} {{11-23=}}

print("[\(x, radix: x)]")
// CHECK: [(1, radix: 1)]
// expected-warning@-2{{interpolating multiple values will not form a tuple in future versions of Swift}}
// expected-note@-3{{insert parentheses to keep current behavior}} {11-11(}} {{25-25=)}}

print("[\(stringInterpolationSegment: x)]")
// CHECK: [1]
// expected-warning@-2{{labeled interpolations will not be ignored in future versions of Swift}}
// expected-note@-3{{remove 'stringInterpolationSegment' label to keep current behavior}} {{11-39=}}
