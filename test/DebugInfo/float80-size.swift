// RUN: %target-swift-frontend %s -Onone -emit-ir -gdwarf-types -o - | %FileCheck %s

// REQUIRES: CPU=i386 || CPU=x86_64
//
// Windows and Android do not expose Float80.
// UNSUPPORTED: OS=windows-msvc, OS=linux-android

// Builtin.FPIEEE80 holds an 80-bit value in wider storage: 128 bits on x86_64,
// 96 bits on i386. The debug info describes the storage, so that a consumer
// laying out a type that contains it (Float80 here) arrives at the same size
// the runtime does.

let float80 = Float80(1.0625)

// CHECK-DAG: !DIBasicType(name: "$sBf80_D", size: {{96|128}}, encoding: DW_ATE_float)
// CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "Float80", {{.*}}size: {{96|128}},
