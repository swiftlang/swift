// RUN: %target-swift-frontend %s -Onone -emit-ir -gdwarf-types -o - | %FileCheck %s

// File is empty as this test check for the builtin stdlib types that should 
// always be emitted.

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBoD", 
// CHECK-SAME: size: {{64|32}}, num_extra_inhabitants: {{2147483647|4096}}, flags: DIFlagArtificial, 
// CHECK-SAME: runtimeLang: DW_LANG_Swift)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$syXlD", 
// CHECK-SAME: size: {{64|32}},

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBbD", 
// CHECK-SAME: size: {{64|32}}, num_extra_inhabitants: {{2147483647|4096}}, flags: DIFlagArtificial, 
// CHECK-SAME: runtimeLang: DW_LANG_Swift)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sBpD", 
// CHECK-SAME: size: {{64|32}}, num_extra_inhabitants: 1, flags: DIFlagArtificial, 
// CHECK-SAME: runtimeLang: DW_LANG_Swift)

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$syyXfD", 
// CHECK-SAME: size: {{64|32}}, 
// CHECK-SAME: runtimeLang: DW_LANG_Swift

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "$sypXpD", size: {{64|32}},
// CHECK-SAME: flags: DIFlagArtificial, runtimeLang: DW_LANG_Swift, identifier: "$sypXpD")


