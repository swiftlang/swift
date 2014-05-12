// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %s -target x86_64-apple-darwin13 -sdk %S/Inputs -I=%S/Inputs/custom-modules -module-cache-path %t/clang-module-cache -emit-ir -o %t/without-adapter.ll
// RUN: FileCheck %s < %t/without-adapter.ll

// RUN: %swift -emit-module %S/Inputs/adapter.swift -sdk %S/Inputs -module-link-name SwiftAdapter -module-name ClangModuleWithAdapter -I=%S/Inputs/custom-modules -o %t
// RUN: %swift %s -target x86_64-apple-darwin13 -sdk %S/Inputs -I=%S/Inputs/custom-modules -I %t -module-cache-path %t/clang-module-cache -emit-ir -o %t/with-adapter.ll
// RUN: FileCheck %s < %t/with-adapter.ll
// RUN: FileCheck --check-prefix=CHECK-WITH-SWIFT %s < %t/with-adapter.ll

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

var _ = LinkFramework.IComeFromLinkFramework
UsesSubmodule.useSomethingFromSubmodule()

// CHECK: !{{[0-9]+}} = metadata !{i32 6, metadata !"Linker Options", metadata ![[LINK_LIST:[0-9]+]]}
// CHECK: ![[LINK_LIST]] = metadata !{

// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lLock"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lStock"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"LinkFramework"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-lUnderlyingClangLibrary"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"Indirect"}
// CHECK-DAG: !{{[0-9]+}} = metadata !{metadata !"-framework", metadata !"HasSubmodule"}

// CHECK-WITH-SWIFT: !{{[0-9]+}} = metadata !{metadata !"-lSwiftAdapter"}
