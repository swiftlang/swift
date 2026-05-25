// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// Verify that an existential containing ONLY inverse requirements
// (no parameterized protocols) does NOT emit a call to
// swift_getExtendedExistentialTypeMetadata, which is unavailable on
// older OS deployments and breaks iOS 15 / earlier.

// CHECK-LABEL: define {{.*}} @"$s4test3fooyyF"
// CHECK-NOT: swift_getExtendedExistentialTypeMetadata
public func foo() {
    let _: any (Any & ~Copyable & ~Escapable).Type = (Any).self as Any.Type
}
