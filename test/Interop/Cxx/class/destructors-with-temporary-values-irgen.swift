// RUN: %target-swiftxx-frontend -emit-ir -I %S/Inputs -validate-tbd-against-ir=none %s | %FileCheck %s

import DestructorsWithTemporaryValues

public func test() {
  testFunction()
}

// Make sure that we emit IR for functions that are called from the custom
// destructor of a temporary value created on the C++ side.

// CHECK: define{{( dso_local)?}} void @{{_Z22referencedByDestructorP5Value|"\?referencedByDestructor@@YAXPEAUValue@@@Z"}}
// CHECK: define linkonce_odr{{( dso_local)?}} void @{{_ZN5Value22referencedByDestructorEv|"\?referencedByDestructor@Value@@QEAAXXZ"}}
