// RUN: %target-swift-frontend %s -emit-ir -disable-availability-checking -g -o - | %FileCheck %s

let a: ([3 of Int], InlineArray<3, Int>) = ([1, 2, 3], [1, 2, 3])
let b: ([3 of [1 of String]], InlineArray<3, InlineArray<1, String>>) = ([[""], [""], [""]], [[""], [""], [""]])

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s$2_SiXSA_s11InlineArrayVy$2_SiGtD", {{.*}})
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$s$2_$0_SSXSAXSA_s11InlineArrayVy$2_ABy$0_SSGGtD", {{.*}})

// RUN: swift-demangle 's$2_SiXSA_s11InlineArrayVy$2_SiGtD' | %FileCheck %s --check-prefix SIMPLE
// SIMPLE: ([3 of Swift.Int], Swift.InlineArray<3, Swift.Int>)

// RUN: swift-demangle 's$2_$0_SSXSAXSA_s11InlineArrayVy$2_ABy$0_SSGGtD' | %FileCheck %s --check-prefix NESTED
// NESTED: ([3 of [1 of Swift.String]], Swift.InlineArray<3, Swift.InlineArray<1, Swift.String>>)
