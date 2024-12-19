// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -disable-availability-checking -diagnostic-style llvm %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import CxxForeignRef

// REQUIRES: objc_interop

func testObjCMethods() {
    var frt1 = Bridge.objCMethodReturningFRTUnannotated()
    // CHECK: objc_method {{.*}} #Bridge.objCMethodReturningFRTUnannotated!foreign : (Bridge.Type) -> () -> CxxRefType, $@convention(objc_method) (@objc_metatype Bridge.Type) -> CxxRefType

    var frt2 = Bridge.objCMethodReturningFRTUnowned()
    // CHECK: objc_method {{.*}} #Bridge.objCMethodReturningFRTUnowned!foreign : (Bridge.Type) -> () -> CxxRefType, $@convention(objc_method) (@objc_metatype Bridge.Type) -> CxxRefType

    var frt3 = Bridge.objCMethodReturningFRTOwned()
    // CHECK: objc_method {{.*}} #Bridge.objCMethodReturningFRTOwned!foreign : (Bridge.Type) -> () -> CxxRefType, $@convention(objc_method) (@objc_metatype Bridge.Type) -> @owned CxxRefType
 }
