// RUN: %target-swift-frontend -module-name test -emit-ir %s -Xcc -fno-builtin | %FileCheck %s
// RUN: %target-swift-frontend -module-name test -emit-ir %s | %FileCheck %s --check-prefix=NEGATIVE

// CHECK: define {{.*}}swiftcc void @"$s4test3fooyyF"() [[ATTRS:#[0-9]+]]
public func foo() {}

// CHECK: [[ATTRS]] = {{{.*}}"no-builtins"

// Without -fno-builtin, the attribute must not be added.
// NEGATIVE-NOT: "no-builtins"
