// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -I %S%{fs-sep}Inputs %s \
// RUN:   -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}inherit-frt.hpp

import InheritFRT

let _ = makeSimpleValue()
let _ = makeSimpleShared()
let _ = makeSimpleImmortal()

let _ = makeSingleShared_Shared()
let _ = makeSingleShared_NoAttr()
let _ = makeSingleShared_Shared_Final()
let _ = makeSingleShared_NoAttr_Final()

let _ = makeSingleShared_Shared_Shared()
let _ = makeSingleShared_Shared_NoAttr()
let _ = makeSingleShared_NoAttr_Shared()
let _ = makeSingleShared_NoAttr_NoAttr()

let _ = makeSingleImmortal_Immort()
let _ = makeSingleImmortal_NoAttr()
let _ = makeSingleImmortal_Immort_Final()
let _ = makeSingleImmortal_NoAttr_Final()

let _ = makeSingleImmortal_Immort_Immort()
let _ = makeSingleImmortal_Immort_NoAttr()
let _ = makeSingleImmortal_NoAttr_Immort()
let _ = makeSingleImmortal_NoAttr_NoAttr()

let _ = makeOverloadShared_Shared()
let _ = makeOverloadShared_Shared_Shared()
let _ = makeOverloadShared_Shared_NoAttr()

let _ = makeOneShared_RU_Shared()
let _ = makeOneShared_UR_Shared()
let _ = makeOneShared_RU_NoAttr()
let _ = makeOneShared_UR_NoAttr()
let _ = makeOneShared_DRU_Shared()
let _ = makeOneShared_UDR_Shared()
let _ = makeOneShared_DRU_NoAttr()
let _ = makeOneShared_UDR_NoAttr()

let _ = makeTwoShared_NoAttr() // NOTE: this is not a valid FRT
let _ = makeTwoShared_Shared()
let _ = makeTwoShared_Shared_UsingA()
let _ = makeTwoShared_Shared_UsingB()

let _ = makeDiamondRef_NoAttr() // NOTE: this is not a valid FRT
let _ = makeDiamondRef_Shared()
let _ = makeDiamondRef_VV_NoAttr()
let _ = makeDiamondRef_VV_Shared()
let _ = makeDiamondRef_XV_NoAttr() // NOTE: this is not a valid FRT
let _ = makeDiamondRef_VX_NoAttr() // NOTE: this is not a valid FRT
let _ = makeDiamondRef_XV_Shared()
let _ = makeDiamondRef_VX_Shared()

let _ = makeDiamondNoRef_ARB()
let _ = makeDiamondNoRef_RAB()
let _ = makeDiamondNoRef_RARB() // NOTE: this is not a valid FRT
