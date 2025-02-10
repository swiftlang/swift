// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift -verify-additional-file %S/Inputs/cxx-frt.h -Xcc -Wno-return-type

import CxxForeignRef

// REQUIRES: objc_interop
func testObjCMethods() {
    _ = Bridge.objCMethodReturningFRTBothAnnotations()
    _ = Bridge.objCMethodReturningNonCxxFrtAnannotated()
    _ = Bridge.objCMethodReturningFRTUnannotated()
}
