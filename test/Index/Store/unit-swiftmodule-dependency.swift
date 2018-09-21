// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -index-store-path %t/idx %S/Inputs/SwiftModuleA.swift -emit-module -o %t/SwiftModuleA.swiftmodule
// RUN: %target-swift-frontend -index-store-path %t/idx %S/Inputs/SwiftModuleB.swift -emit-module -o %t/SwiftModuleB.swiftmodule -I %t

// RUN: echo 'import SwiftModuleA' > %t/s2.swift
// RUN: %target-swift-frontend -index-store-path %t/idx %s %t/s2.swift -c -o %t/s1.o -o %t/s2.o -I %t -emit-module -module-name main -emit-module-path %t/main.swiftmodule

// RUN: c-index-test core -print-unit %t/idx | %FileCheck %s

import SwiftModuleA
import SwiftModuleB

func test() {
  funcSwiftA()
  funcSwiftB()
}

// CHECK: [[MODA:SwiftModuleA.swiftmodule-[A-Z0-9]*]]
// CHECK: --------
// CHECK: has-main: 1
// CHECK: out-file: {{.*}}/SwiftModuleA.swiftmodule
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{.*}}/Swift.swiftmodule
// CHECK: DEPEND END

// CHECK: [[MODB:SwiftModuleB.swiftmodule-[A-Z0-9]*]]
// CHECK: --------
// CHECK: has-main: 1
// CHECK: out-file: {{.*}}/SwiftModuleB.swiftmodule
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{.*}}/Swift.swiftmodule
// CHECK: Unit | user | SwiftModuleA | {{.*}}/SwiftModuleA.swiftmodule
// CHECK: DEPEND END

// CHECK-NOT: main.swiftmodule-

// CHECK: s1.o-
// CHECK: --------
// CHECK: has-main: 1
// CHECK: out-file: {{.*}}/s1.o
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{.*}}/Swift.swiftmodule
// CHECK: Unit | user | SwiftModuleA | {{.*}}/SwiftModuleA.swiftmodule
// CHECK: Unit | user | SwiftModuleB | {{.*}}/SwiftModuleB.swiftmodule
// CHECK: DEPEND END

// CHECK: s2.o-
// CHECK: --------
// CHECK: has-main: 1
// CHECK: out-file: {{.*}}/s2.o
// CHECK: DEPEND START
// CHECK: Unit | system | Swift | {{.*}}/Swift.swiftmodule
// CHECK: Unit | user | SwiftModuleA | {{.*}}/SwiftModuleA.swiftmodule
// CHECK: DEPEND END
