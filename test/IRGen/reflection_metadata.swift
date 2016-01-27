// RUN: %target-swift-frontend -enable-reflection-metadata -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s
// RUN: %target-swift-frontend -enable-reflection-metadata -strip-reflection-names -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s --check-prefix=STRIP_REFLECTION_NAMES
// RUN: %target-swift-frontend -enable-reflection-metadata -strip-reflection-metadata -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s --check-prefix=STRIP_REFLECTION_METADATA

// STRIP_REFLECTION_NAMES-NOT: {{.*}}__swift3_reflstr
// STRIP_REFLECTION_NAMES_DAG: {{.*}}__swift3_reflect
// STRIP_REFLECTION_NAMES-DAG: @"\01l__swift3_reflection_metadata"

// STRIP_REFLECTION_METADATA-NOT: {{.*}}__swift3_reflstr
// STRIP_REFLECTION_METADATA-NOT: {{.*}}__swift3_reflect
// STRIP_REFLECTION_METADATA-NOT: @"\01l__swift3_reflection_metadata"

// Field names

// CHECK-DAG: private constant [2 x i8] c"i\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"ms\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"me\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"mc\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [2 x i8] c"C\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [2 x i8] c"S\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [2 x i8] c"E\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [2 x i8] c"I\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [2 x i8] c"t\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [4 x i8] c"mgs\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [4 x i8] c"mge\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [4 x i8] c"mgc\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"GC\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"GS\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"
// CHECK-DAG: private constant [3 x i8] c"GE\00", section "__DATA, __swift3_reflstr, coalesced, no_dead_strip"

// Type references

// CHECK-DAG: @"\01l__swift3_reflection_metadata" = private constant <{ {{.*}} }> <{
// CHECK: i32 3,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @21 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 8)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @22 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 12)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([32 x i8]* @23 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 16)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @24 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 20)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([11 x i8]* @25 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 24)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @26 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 28)) to i32),
// CHECK: i32 3,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @21 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 40)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @22 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 44)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([12 x i8]* @27 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 48)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @28 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 52)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([4 x i8]* @29 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 56)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @26 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 60)) to i32),
// CHECK: i32 4,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([13 x i8]* @30 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 72)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @31 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 76)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([13 x i8]* @32 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 80)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @33 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 84)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([13 x i8]* @34 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 88)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @35 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 92)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([12 x i8]* @36 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 96)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @37 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 100)) to i32),
// CHECK: i32 4,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @38 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 112)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @39 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 116)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([24 x i8]* @40 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 120)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @22 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 124)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([24 x i8]* @41 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 128)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([4 x i8]* @42 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 132)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([22 x i8]* @43 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 136)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([4 x i8]* @44 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 140)) to i32),
// CHECK: i32 4,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @38 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 152)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @39 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 156)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([6 x i8]* @45 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 160)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @22 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 164)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([23 x i8]* @46 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 168)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([4 x i8]* @47 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 172)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([7 x i8]* @48 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 176)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([4 x i8]* @44 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 180)) to i32),
// CHECK: i32 4,
// CHECK: i32 8, 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([29 x i8]* @49 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 192)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @50 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 196)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([29 x i8]* @51 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 200)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @52 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 204)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([29 x i8]* @53 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 208)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([3 x i8]* @54 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 212)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([25 x i8]* @55 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 216)) to i32), 
// CHECK: i32 trunc (i64 sub (i64 ptrtoint ([2 x i8]* @37 to i64), i64 add (i64 ptrtoint (<{ {{.*}} }>* @"\01l__swift3_reflection_metadata" to i64), i64 220)) to i32) }>,
// CHECK: section "__DATA, __swift3_reflect, regular, no_dead_strip", align 8

