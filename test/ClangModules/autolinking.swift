// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -emit-ir -o %t/without-adapter.ll
// RUN: FileCheck %s < %t/without-adapter.ll

// RUN: %target-swift-frontend -emit-module %S/Inputs/adapter.swift -sdk %S/Inputs -module-link-name SwiftAdapter -module-name ClangModuleWithAdapter -I %S/Inputs/custom-modules -o %t
// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -I %t -emit-ir -o %t/with-adapter.ll
// RUN: FileCheck %s < %t/with-adapter.ll
// RUN: FileCheck --check-prefix=CHECK-WITH-SWIFT %s < %t/with-adapter.ll

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

var _ = LinkFramework.IComeFromLinkFramework
UsesSubmodule.useSomethingFromSubmodule()

// CHECK: !{{[0-9]+}} = !{i32 6, !"Linker Options", ![[LINK_LIST:[0-9]+]]}
// CHECK: ![[LINK_LIST]] = !{

// CHECK-DAG: !{{[0-9]+}} = !{!"-lLock"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-lStock"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-lUnderlyingClangLibrary"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Indirect"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"HasSubmodule"}

// CHECK-WITH-SWIFT: !{{[0-9]+}} = !{!"-lSwiftAdapter"}
