// RUN: not %target-swift-frontend -typecheck -verify %s 2>&1 | %FileCheck %s --implicit-check-not error: --implicit-check-not note: --implicit-check-not warning:

typealias FnType = () -> ()

// Positive case: the diagnostic belongs to the expected group.
extension FnType {} // expected-error {{non-nominal type 'FnType' (aka '() -> ()') cannot be extended}} {{group-name=NominalTypes}}

// Wrong group name: the diagnostic belongs to a different group.
extension FnType {} // expected-error {{non-nominal type 'FnType' (aka '() -> ()') cannot be extended}} {{group-name=FooBarBaz}}
// CHECK: error: expected group name not seen; actual group name: {{[{][{]}}group-name=NominalTypes{{[}][}]}}

// One of multiple expected group names doesn't match.
extension FnType {} // expected-error {{non-nominal type 'FnType' (aka '() -> ()') cannot be extended}} {{group-name=NominalTypes}} {{group-name=FooBarBaz}}
// CHECK: error: expected group name not seen{{$}}

// Diagnostic has no group at all.
let x: Int = "hello, world!" // expected-error {{cannot convert value of type 'String' to specified type 'Int'}} {{group-name=NominalTypes}}
// CHECK: error: expected group name not seen
