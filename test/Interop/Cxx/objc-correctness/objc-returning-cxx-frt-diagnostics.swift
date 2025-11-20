// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -Xcc -Wno-return-type \
// RUN:   -I %S/Inputs %s \
// RUN:   -verify-additional-file %S/Inputs/cxx-frt.h

// REQUIRES: objc_interop

import CxxForeignRef

func testObjCMethods() {
    _ = Bridge.objCMethodReturningFRTBothAnnotations()
    _ = Bridge.objCMethodReturningNonCxxFrtAnannotated()
    _ = Bridge.objCMethodReturningFRTUnannotated()
    // expected-warning@-1 {{cannot infer ownership of foreign reference value returned by 'objCMethodReturningFRTUnannotated()'}}
}
