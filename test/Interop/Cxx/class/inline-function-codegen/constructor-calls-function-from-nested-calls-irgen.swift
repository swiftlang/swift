// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ConstructorCallsFunctionFromNestedCalls

let a = Hold42WithLongInitCallGraph()

// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z11get42Level1v|"\?get42Level1@@YAHXZ"}}
// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z11get42Level2v|"\?get42Level2@@YAHXZ"}}
// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z11get42Level3v|"\?get42Level3@@YAHXZ"}}
// CHECK-DAG: define linkonce_odr{{( dso_local)?}} i32 @{{_Z11get42Level4v|"\?get42Level4@@YAHXZ"}}
