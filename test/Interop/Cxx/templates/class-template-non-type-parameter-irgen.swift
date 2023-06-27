// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import ClassTemplateNonTypeParameter

let p = MagicIntPair()
let t = MagicIntTriple()

// CHECK: @"${{s4main1pSo36__CxxTemplateInst10MagicArrayIiLm2EEVvp|s4main1pSo36__CxxTemplateInst10MagicArrayIiLy2EEVvp}}"
// CHECK: @"${{s4main1tSo36__CxxTemplateInst10MagicArrayIiLm3EEVvp|s4main1tSo36__CxxTemplateInst10MagicArrayIiLy3EEVvp}}"
