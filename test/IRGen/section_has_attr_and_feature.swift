// RUN: %target-swift-frontend -primary-file %s -emit-ir -parse-as-library | %FileCheck %s

#if hasAttribute(section)
@section("__DATA,__mysection") var g0: Int = 1
#endif

// Verify that the language feature that we revived still works as well.
#if hasFeature(SymbolLinkageMarkers)
@section("__DATA,__mysection") var g1: (Int, Int) = (42, 43)
#endif

// CHECK:  @"$s28section_has_attr_and_feature2g0Sivp" = hidden global %TSi <{ {{(i64|i32)}} 1 }>, section "__DATA,__mysection"
// CHECK:  @"$s28section_has_attr_and_feature2g1Si_Sitvp" = hidden global <{ %TSi, %TSi }> <{ %TSi <{ {{(i64|i32)}} 42 }>, %TSi <{ {{(i64|i32)}} 43 }> }>, section "__DATA,__mysection"
