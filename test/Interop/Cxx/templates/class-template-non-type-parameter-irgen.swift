// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ClassTemplateNonTypeParameter

let p = MagicIntPair()
let t = MagicIntTriple()

// CHECK: @"${{s4main1pSo0042MagicArrayCInt_CUnsignedLong_2_DFABlHknrCcVvp|s4main1pSo0047MagicArrayCInt_CUnsignedLongLong_2_ofBJmlmartjdVvp}}"
// CHECK: @"${{s4main1tSo0042MagicArrayCInt_CUnsignedLong_3_DFABlHknrCcVvp|s4main1tSo0047MagicArrayCInt_CUnsignedLongLong_3_ofBJmlmartjdVvp}}"
