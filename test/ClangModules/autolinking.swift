// RUN: %swift -sdk=%S/Inputs -I=%S/Inputs/custom-modules %s -emit-llvm -o - | FileCheck %s

import LinkMusket
import LinkFramework

var _ = LinkFramework.IComeFromLinkFramework

// CHECK: !{{[0-9]+}} = metadata !{i32 6, metadata !"Linker Options", metadata ![[LINK_LIST:[0-9]+]]}
// CHECK: ![[LINK_LIST]] = metadata !{
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lLock"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lStock"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"LinkFramework"}
