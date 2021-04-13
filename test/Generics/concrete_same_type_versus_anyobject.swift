// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
struct S {}
class C {}

struct G1<T : AnyObject> {}

// CHECK-LABEL: Generic signature: <T where T == S>
extension G1 where T == S {}
// expected-error@-1 {{'T' requires that 'S' be a class type}}
// expected-note@-2 {{same-type constraint 'T' == 'S' implied here}}

// CHECK-LABEL: Generic signature: <T where T == C>
extension G1 where T == C {}

struct G2<U> {}

// CHECK-LABEL: Generic signature: <U where U == S>
extension G2 where U == S, U : AnyObject {}
// expected-error@-1 {{'U' requires that 'S' be a class type}}
// expected-note@-2 {{same-type constraint 'U' == 'S' implied here}}
// expected-note@-3 {{constraint 'U' : 'AnyObject' implied here}}

// CHECK-LABEL: Generic signature: <U where U == C>
extension G2 where U == C, U : AnyObject {}
// expected-warning@-1 {{redundant constraint 'U' : 'AnyObject'}}
// expected-note@-2 {{constraint 'U' : 'AnyObject' implied here}}

// CHECK-LABEL: Generic signature: <U where U : C>
extension G2 where U : C, U : AnyObject {}
// expected-warning@-1 {{redundant constraint 'U' : 'AnyObject'}}
// expected-note@-2 {{constraint 'U' : 'AnyObject' implied here}}
