// RUN: rm -rf %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs  %s -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature WarnUnannotatedReturnOfCxxFrt -verify-additional-file %S/Inputs/cxx-frt.h -Xcc -Wno-return-type

// REQUIRES: swift_feature_WarnUnannotatedReturnOfCxxFrt

import CxxForeignRef

// REQUIRES: objc_interop
func testObjCMethods() {
    _ = Bridge.objCMethodReturningFRTBothAnnotations()
    _ = Bridge.objCMethodReturningNonCxxFrtAnannotated()
    _ = Bridge.objCMethodReturningFRTUnannotated() // expected-warning {{cannot infer the ownership of the returned value, annotate 'objCMethodReturningFRTUnannotated()' with either SWIFT_RETURNS_RETAINED or SWIFT_RETURNS_UNRETAINED}}
}
