// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -resolve-imports %s -emit-module-interface-path %t/main.swiftinterface -enable-library-evolution
// RUN: %FileCheck %s < %t/main.swiftinterface

// CHECK: import Swift

// CHECK: public func f() -> Swift.Int
public func f() -> Int {}

// Deliberate semantic errors to ensure private and internal declarations don't
// get type checked.
private func bad1(_: NotAThing) -> DoesNotExist {}

internal typealias Bad1 = NotEvenReal.DefinitelyNot

// Destructors
public class C {}

// CHECK: public class C {
// CHECK: deinit
// CHECK: }