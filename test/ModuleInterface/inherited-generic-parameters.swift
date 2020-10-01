// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// RUN: %target-swift-frontend -emit-module -module-name main -primary-file %s -emit-module-path %t/main~partial.swiftmodule -enable-library-evolution

// RUN: %target-swift-frontend -merge-modules %t/main~partial.swiftmodule -emit-module-path %t/main.swiftmodule -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// This test makes sure that we substitute uses of the superclass's generic
// parameters when we inherit initializers.

// CHECK: public class Base<In, Out> {
public class Base<In, Out> {
// CHECK-NEXT: public init(x: @escaping (In) -> Out)
  public init(x: @escaping (In) -> Out) {}

// CHECK-NEXT: public init<A>(_: A, _: A)
  public init<A>(_: A, _: A) {}

// CHECK-NEXT: public init<C>(_: C) where C : main.Base<In, Out>
  public init<C>(_: C) where C : Base<In, Out> {}
// CHECK: }
}

// CHECK: public class Derived<T> : {{(main.)?}}Base<T, T> {
public class Derived<T> : Base<T, T> {
// CHECK-NEXT: {{(@objc )?}}deinit
// CHECK-NEXT: override public init(x: @escaping (T) -> T)
// CHECK-NEXT: override public init<A>(_ argument: A, _ argument: A)
// CHECK-NEXT: override public init<C>(_ argument: C) where C : main.Base<T, T>
// CHECK-NEXT: }
}

