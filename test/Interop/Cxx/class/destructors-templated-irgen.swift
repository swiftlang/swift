// RUN: %target-swiftxx-frontend -emit-ir -I %S/Inputs -validate-tbd-against-ir=none %s | %FileCheck %s

import Destructors

let _ = DerivedTemplatedHasVirtualDestructorChar()

// CHECK: define {{.*}} @{{_ZN36DerivedTemplatedHasVirtualDestructorIcED2Ev|"\?\?1\?\$DerivedTemplatedHasVirtualDestructor@D@@UEAA@XZ"}}
// CHECK: entry:
// CHECK:   call {{.*}} @{{_ZN29TemplatedHasVirtualDestructorIcED2Ev|"\?\?1\?\$TemplatedHasVirtualDestructor@D@@UEAA@XZ"}}
