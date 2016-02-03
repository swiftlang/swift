// RUN: %target-swift-frontend -enable-reflection-metadata -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s
// RUN: %target-swift-frontend -enable-reflection-metadata -strip-reflection-names -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s --check-prefix=STRIP_REFLECTION_NAMES
// RUN: %target-swift-frontend -enable-reflection-metadata -strip-reflection-metadata -emit-ir %S/Inputs/reflection_metadata.swift | FileCheck %s --check-prefix=STRIP_REFLECTION_METADATA

// STRIP_REFLECTION_NAMES-NOT: {{.*}}swift3_reflstr
// STRIP_REFLECTION_NAMES_DAG: {{.*}}swift3_reflect
// STRIP_REFLECTION_NAMES-DAG: @"\01l__swift3_reflection_metadata"

// STRIP_REFLECTION_METADATA-NOT: {{.*}}swift3_reflstr
// STRIP_REFLECTION_METADATA-NOT: {{.*}}swift3_reflect
// STRIP_REFLECTION_METADATA-NOT: @"\01l__swift3_reflection_metadata"

// CHECK-DAG: private constant [2 x i8] c"i\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"ms\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"me\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"mc\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [2 x i8] c"C\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [2 x i8] c"S\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [2 x i8] c"E\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [2 x i8] c"I\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [2 x i8] c"t\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [4 x i8] c"mgs\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [4 x i8] c"mge\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [4 x i8] c"mgc\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"GC\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"GS\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"
// CHECK-DAG: private constant [3 x i8] c"GE\00", section "{{[^"]*}}swift3_reflstr{{[^"]*}}"

// CHECK: @"\01l__swift3_reflection_metadata" = private constant <{ {{.*}} }>
