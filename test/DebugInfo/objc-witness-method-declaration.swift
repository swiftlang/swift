// REQUIRES: objc_interop
// RUN: %target-swift-frontend -primary-file %s -emit-ir -gdwarf-types -o - | %FileCheck %s

// Verify that we added a declaration for a witness method.

// CHECK: define {{.*}}"$s4main9SomeClassC3fooyyFTo"{{.*}} !dbg ![[INIT_DEF_DBG:[0-9]+]]
// CHECK: ![[INIT_DEF_DBG]] = distinct !DISubprogram(name: "foo", linkageName: "$s4main9SomeClassC3fooyyFTo"
// CHECK-SAME: DISPFlagDefinition{{.*}} declaration: ![[FUNC_DEF_DBG:[0-9]+]]
// CHECK: ![[FUNC_DEF_DBG]] = !DISubprogram(name: "foo", linkageName: "$s4main9SomeClassC3fooyyFTo"

import Foundation

class SomeClass {
    public required init() {}

    @objc func foo() {}
}
