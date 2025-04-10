// RUN: %target-swift-frontend %s -emit-ir -enable-experimental-feature InlineArrayTypeSugar -disable-availability-checking -g -o - | %FileCheck %s

// REQUIRES: swift_feature_InlineArrayTypeSugar

let a: ([3 x Int], InlineArray<3, Int>) = ([1, 2, 3], [1, 2, 3])
let b: ([3 x [1 x String]], InlineArray<3, InlineArray<1, String>>) = ([[""], [""], [""]], [[""], [""], [""]])

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s$2_SiXSA_s11InlineArrayVy$2_SiGtD", {{.*}})
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s$2_$0_SSXSAXSA_s11InlineArrayVy$2_ABy$0_SSGGtD", {{.*}})

// RUN: swift-demangle 's$2_SiXSA_s11InlineArrayVy$2_SiGtD' | %FileCheck %s --check-prefix SIMPLE
// SIMPLE: ([3 x Swift.Int], Swift.InlineArray<3, Swift.Int>)

// RUN: swift-demangle 's$2_$0_SSXSAXSA_s11InlineArrayVy$2_ABy$0_SSGGtD' | %FileCheck %s --check-prefix NESTED
// NESTED: ([3 x [1 x Swift.String]], Swift.InlineArray<3, Swift.InlineArray<1, Swift.String>>)
