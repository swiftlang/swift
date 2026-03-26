// RUN: %target-swift-emit-ir -Onone %s -I %S/Inputs -cxx-interoperability-mode=default -Xcc -fignore-exceptions \
// RUN:   | %FileCheck %s

// This test ensures that we use the correct retain/release functions for FRTs
// whose reference semantics are inferred via inheritance

// UNSUPPORTED: OS=windows-msvc
// Windows/MSVC mangles symbols differently, and supporting those will only make
// this test more difficult to read and maintain. For now, the main prupose
// of this test is to ensure the correct retain/release functions are being
// selected, so should suffice to only test on non-Windows platforms.

import InheritFRT

let _ = makeSimpleValue()
// CHECK: %[[SimpleValue:.*]] = call ptr @_Z15makeSimpleValuev()
let _ = makeSimpleShared()
// CHECK: %[[SimpleShared:.*]] = call ptr @_Z16makeSimpleSharedv()
// CHECK: call void @_Z19releaseSimpleSharedP12SimpleShared(ptr %[[SimpleShared]])
let _ = makeSimpleImmortal()
// CHECK: %[[SimpleImmortal:.*]] = call ptr @_Z18makeSimpleImmortalv()

let _ = makeSingleShared_Shared()
// CHECK: %[[SingleShared_Shared:.*]] = call ptr @_Z23makeSingleShared_Sharedv()
// CHECK: call void @_Z26releaseSingleShared_SharedP19SingleShared_Shared(ptr %[[SingleShared_Shared]])
let _ = makeSingleShared_NoAttr()
// CHECK: %[[SingleShared_NoAttr:.*]] = call ptr @_Z23makeSingleShared_NoAttrv()
// CHECK: call void @_ZN19SingleShared_NoAttr55__synthesized_lifetimeAccessor_releaseSingleShared_BaseEv(ptr %[[SingleShared_NoAttr]])
let _ = makeSingleShared_Shared_Final()
// CHECK: %[[SingleShared_Shared_Final:.*]] = call ptr @_Z29makeSingleShared_Shared_Finalv()
// CHECK: call void @_Z32releaseSingleShared_Shared_FinalP25SingleShared_Shared_Final(ptr %[[SingleShared_Shared_Final]])
let _ = makeSingleShared_NoAttr_Final()
// CHECK: %[[SingleShared_NoAttr_Final:.*]] = call ptr @_Z29makeSingleShared_NoAttr_Finalv()
// CHECK: call void @_ZN25SingleShared_NoAttr_Final55__synthesized_lifetimeAccessor_releaseSingleShared_BaseEv(ptr %[[SingleShared_NoAttr_Final]])

let _ = makeSingleShared_Shared_Shared()
// CHECK: %[[SingleShared_Shared_Shared:.*]] = call ptr @_Z30makeSingleShared_Shared_Sharedv()
// CHECK: call void @_Z33releaseSingleShared_Shared_SharedP26SingleShared_Shared_Shared(ptr %[[SingleShared_Shared_Shared]])
// let _ = makeSingleShared_Shared_NoAttr() // BUG: should be allowed
let _ = makeSingleShared_NoAttr_Shared()
// CHECK: %[[SingleShared_NoAttr_Shared:.*]] = call ptr @_Z30makeSingleShared_NoAttr_Sharedv()
// CHECK: call void @_Z33releaseSingleShared_NoAttr_SharedP26SingleShared_NoAttr_Shared(ptr %[[SingleShared_NoAttr_Shared]])
let _ = makeSingleShared_NoAttr_NoAttr()
// CHECK: %[[SingleShared_NoAttr_NoAttr:.*]] = call ptr @_Z30makeSingleShared_NoAttr_NoAttrv()
// CHECK: call void @_ZN26SingleShared_NoAttr_NoAttr55__synthesized_lifetimeAccessor_releaseSingleShared_BaseEv(ptr %[[SingleShared_NoAttr_NoAttr]])

let _ = makeSingleImmortal_Immort()
// CHECK: %[[SingleImmortal_Immort:.*]] = call ptr @_Z25makeSingleImmortal_Immortv()
let _ = makeSingleImmortal_NoAttr()
// CHECK: %[[SingleImmortal_NoAttr:.*]] = call ptr @_Z25makeSingleImmortal_NoAttrv()
let _ = makeSingleImmortal_Immort_Final()
// CHECK: %[[SingleImmortal_Immort_Final:.*]] = call ptr @_Z31makeSingleImmortal_Immort_Finalv()
let _ = makeSingleImmortal_NoAttr_Final()
// CHECK: %[[SingleImmortal_NoAttr_Final:.*]] = call ptr @_Z31makeSingleImmortal_NoAttr_Finalv()

let _ = makeSingleImmortal_Immort_Immort()
// CHECK: %[[SingleImmortal_Immort_Immort:.*]] = call ptr @_Z32makeSingleImmortal_Immort_Immortv()
let _ = makeSingleImmortal_Immort_NoAttr()
// CHECK: %[[SingleImmortal_Immort_NoAttr:.*]] = call ptr @_Z32makeSingleImmortal_Immort_NoAttrv()
let _ = makeSingleImmortal_NoAttr_Immort()
// CHECK: %[[SingleImmortal_NoAttr_Immort:.*]] = call ptr @_Z32makeSingleImmortal_NoAttr_Immortv()
let _ = makeSingleImmortal_NoAttr_NoAttr()
// CHECK: %[[SingleImmortal_NoAttr_NoAttr:.*]] = call ptr @_Z32makeSingleImmortal_NoAttr_NoAttrv()

// BUG: this is probably miscompiling. We are not correctly accounting for struct layout.
let _ = makeOverloadShared_Shared()
// CHECK: %[[OverloadShared_Shared:.*]] = call ptr @_Z25makeOverloadShared_Sharedv()
// CHECK: call void @_Z26releaseOverloadShared_BaseP19OverloadShared_Base(ptr %[[OverloadShared_Shared]])
let _ = makeOverloadShared_Shared_Shared()
// CHECK: %[[OverloadShared_Shared_Shared:.*]] = call ptr @_Z32makeOverloadShared_Shared_Sharedv()
// CHECK: call void @_Z26releaseOverloadShared_BaseP19OverloadShared_Base(ptr %[[OverloadShared_Shared_Shared]])
let _ = makeOverloadShared_Shared_NoAttr()
// CHECK: %[[OverloadShared_Shared_NoAttr:.*]] = call ptr @_Z32makeOverloadShared_Shared_NoAttrv()
// CHECK: call void @_Z26releaseOverloadShared_BaseP19OverloadShared_Base(ptr %[[OverloadShared_Shared_NoAttr]])

let _ = makeOneShared_RU_Shared()
// CHECK: %[[OneShared_RU_Shared:.*]] = call ptr @_Z23makeOneShared_RU_Sharedv()
// CHECK: call void @_Z26releaseOneShared_RU_SharedP19OneShared_RU_Shared(ptr %[[OneShared_RU_Shared]])
let _ = makeOneShared_UR_Shared()
// CHECK: %[[OneShared_UR_Shared:.*]] = call ptr @_Z23makeOneShared_UR_Sharedv()
// CHECK: call void @_Z26releaseOneShared_UR_SharedP19OneShared_UR_Shared(ptr %[[OneShared_UR_Shared]])
let _ = makeOneShared_RU_NoAttr()
// CHECK: %[[OneShared_RU_NoAttr:.*]] = call ptr @_Z23makeOneShared_RU_NoAttrv()
// CHECK: call void @_ZN19OneShared_RU_NoAttr49__synthesized_lifetimeAccessor_releaseOneShared_REv(ptr %[[OneShared_RU_NoAttr]])
let _ = makeOneShared_UR_NoAttr()
// CHECK: %[[OneShared_UR_NoAttr:.*]] = call ptr @_Z23makeOneShared_UR_NoAttrv()
// CHECK: call void @_ZN19OneShared_UR_NoAttr49__synthesized_lifetimeAccessor_releaseOneShared_REv(ptr %[[OneShared_UR_NoAttr]])
let _ = makeOneShared_DRU_Shared()
// CHECK: %[[OneShared_DRU_Shared:.*]] = call ptr @_Z24makeOneShared_DRU_Sharedv()
// CHECK: call void @_Z27releaseOneShared_DRU_SharedP20OneShared_DRU_Shared(ptr %[[OneShared_DRU_Shared]])
let _ = makeOneShared_UDR_Shared()
// CHECK: %[[OneShared_UDR_Shared:.*]] = call ptr @_Z24makeOneShared_UDR_Sharedv()
// CHECK: call void @_Z27releaseOneShared_UDR_SharedP20OneShared_UDR_Shared(ptr %[[OneShared_UDR_Shared]])
let _ = makeOneShared_DRU_NoAttr()
// CHECK: %[[OneShared_DRU_NoAttr:.*]] = call ptr @_Z24makeOneShared_DRU_NoAttrv()
// CHECK: call void @_ZN20OneShared_DRU_NoAttr49__synthesized_lifetimeAccessor_releaseOneShared_REv(ptr %[[OneShared_DRU_NoAttr]])
let _ = makeOneShared_UDR_NoAttr()
// CHECK: %[[OneShared_UDR_NoAttr:.*]] = call ptr @_Z24makeOneShared_UDR_NoAttrv()
// CHECK: call void @_ZN20OneShared_UDR_NoAttr49__synthesized_lifetimeAccessor_releaseOneShared_REv(ptr %[[OneShared_UDR_NoAttr]])

// let _ = makeTwoShared_NoAttr() // NOTE: this is not a valid FRT
let _ = makeTwoShared_Shared()
// CHECK: %[[TwoShared_Shared:.*]] = call ptr @_Z20makeTwoShared_Sharedv()
// CHECK: call void @_Z23releaseTwoShared_SharedP16TwoShared_Shared(ptr %[[TwoShared_Shared]])
let _ = makeTwoShared_Shared_UsingA()
// CHECK: %[[TwoShared_Shared_UsingA:.*]] = call ptr @_Z27makeTwoShared_Shared_UsingAv()
// CHECK: call void @_Z18releaseTwoShared_AP11TwoShared_A(ptr %[[TwoShared_Shared_UsingA]])
let _ = makeTwoShared_Shared_UsingB()
// CHECK: %[[TwoShared_Shared_UsingB:.*]] = call ptr @_Z27makeTwoShared_Shared_UsingBv()
// CHECK: call void @_Z18releaseTwoShared_BP11TwoShared_B(ptr %[[TwoShared_Shared_UsingB]])

// let _ = makeDiamondRef_NoAttr() // NOTE: this is not a valid FRT
let _ = makeDiamondRef_Shared()
// CHECK: %[[DiamondRef_Shared:.*]] = call ptr @_Z21makeDiamondRef_Sharedv()
// CHECK: call void @_Z24releaseDiamondRef_SharedP17DiamondRef_Shared(ptr %[[DiamondRef_Shared]])
let _ = makeDiamondRef_VV_NoAttr()
// CHECK: %[[DiamondRef_VV_NoAttr:.*]] = call ptr @_Z24makeDiamondRef_VV_NoAttrv()
// CHECK: call void @_ZN20DiamondRef_VV_NoAttr53__synthesized_lifetimeAccessor_releaseDiamondRef_BaseEv(ptr %[[DiamondRef_VV_NoAttr]])
let _ = makeDiamondRef_VV_Shared()
// CHECK: %[[DiamondRef_VV_Shared:.*]] = call ptr @_Z24makeDiamondRef_VV_Sharedv()
// CHECK: call void @_Z27releaseDiamondRef_VV_SharedP20DiamondRef_VV_Shared(ptr %[[DiamondRef_VV_Shared]])
// let _ = makeDiamondRef_XV_NoAttr() // NOTE: this is not a valid FRT
// let _ = makeDiamondRef_VX_NoAttr() // NOTE: this is not a valid FRT
let _ = makeDiamondRef_XV_Shared()
// CHECK: %[[DiamondRef_XV_Shared:.*]] = call ptr @_Z24makeDiamondRef_XV_Sharedv()
// CHECK: call void @_Z27releaseDiamondRef_XV_SharedP20DiamondRef_XV_Shared(ptr %[[DiamondRef_XV_Shared]])
let _ = makeDiamondRef_VX_Shared()
// CHECK: %[[DiamondRef_VX_Shared:.*]] = call ptr @_Z24makeDiamondRef_VX_Sharedv()
// CHECK: call void @_Z27releaseDiamondRef_VX_SharedP20DiamondRef_VX_Shared(ptr %[[DiamondRef_VX_Shared]])

let _ = makeDiamondNoRef_ARB()
// CHECK: %[[DiamondNoRef_ARB:.*]] = call ptr @_Z20makeDiamondNoRef_ARBv()
// CHECK: call void @_ZN16DiamondNoRef_ARB53__synthesized_lifetimeAccessor_releaseDiamondNoRef_RBEv(ptr %[[DiamondNoRef_ARB]])
let _ = makeDiamondNoRef_RAB()
// CHECK: %[[DiamondNoRef_RAB:.*]] = call ptr @_Z20makeDiamondNoRef_RABv()
// CHECK: call void @_ZN16DiamondNoRef_RAB53__synthesized_lifetimeAccessor_releaseDiamondNoRef_RAEv(ptr %[[DiamondNoRef_RAB]])
// let _ = makeDiamondNoRef_RARB() // NOTE: this is not a valid FRT
