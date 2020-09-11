// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path - %s -swift-version 5 -enable-library-evolution -module-name Module > %t/Module.swiftinterface
// RUN: %target-swift-frontend -emit-silgen %t/Module.swiftinterface | %FileCheck  %s --check-prefix=CHECK-SILGEN
// RUN: not %target-swift-frontend -compile-module-from-interface %t/Module.swiftinterface -o %t/Module.swiftmodule 2>&1 | %FileCheck %s --check-prefix=CHECK-COMPILE

// TF-1094: Derivative registration fails for `.swiftinterface` compilation when
// original `@differentiable` function is serialized but `@derivative` function
// is unserialized.

@inlinable // serialized
@differentiable
@_silgen_name("foo")
public func foo(_ x: Float) -> Float {
  fatalError()
}

@usableFromInline // not serialized
@derivative(of: foo)
@_silgen_name("vjp_foo")
func vjpFoo(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  return (x, { $0 })
}

// Missing differentiability witness VJP entry because `func vjpFoo` is bodiless
// in the `.swiftinterface` file and not lowered to a SIL function.

// CHECK-SILGEN-LABEL: // differentiability witness for foo
// CHECK-SILGEN-NEXT: sil_differentiability_witness [serialized] [parameters 0] [results 0] @foo : $@convention(thin) (Float) -> Float {
// CHECK-SILGEN-NEXT: }

// CHECK-SILGEN-LABEL: sil [serialized] [ossa] @foo
// CHECK-SILGEN-NOT: sil {{.*}} @vjp_foo

// CHECK-COMPILE: Module.swiftinterface:5:2: error: function is not differentiable
// CHECK-COMPILE: Module.swiftinterface:7:24: note: when differentiating this function definition
// CHECK-COMPILE: Module.swiftinterface:9:1: note: missing return for differentiation
