// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s

class Base<T> {}
class Derived : Base<Foo> {}
// expected-error@-1 {{cannot find type 'Foo' in scope}}

// CHECK-LABEL: unify_superclass_types_invalid.(file).f@
// CHECK: Generic signature: <T where T : Derived>
func f<T>(_: T) where T: Base<Int>, T: Derived {}