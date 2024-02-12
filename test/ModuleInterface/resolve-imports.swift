// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -swift-version 5 -parse-as-library -enable-library-evolution -module-name resolve_imports -resolve-imports %s -emit-module-interface-path %t/resolve_imports.swiftinterface
// RUN: %FileCheck %s < %t/resolve_imports.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/resolve_imports.swiftinterface)

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
