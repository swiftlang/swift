// RUN: %swift -emit-llvm -lmagic %s | FileCheck %s

// CHECK: !{{[0-9]+}} = metadata !{i32 6, metadata !"Linker Options", metadata ![[LINK_LIST:[0-9]+]]}
// CHECK: ![[LINK_LIST]] = metadata !{
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lmagic"}
