// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -emit-ir -o - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module %S/Inputs/adapter.swift -sdk %S/Inputs -module-link-name SwiftAdapter -module-name ClangModuleWithAdapter -I %S/Inputs/custom-modules -o %t

// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -I %t -emit-ir -o %t/with-adapter.ll
// RUN: %FileCheck %s < %t/with-adapter.ll
// RUN: %FileCheck -check-prefix CHECK-WITH-SWIFT %s < %t/with-adapter.ll

// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -emit-ir -disable-autolink-framework LinkFramework -o - > %t/with-disabled.ll
// RUN: %FileCheck -check-prefix CHECK-WITH-DISABLED %s < %t/with-disabled.ll
// RUN: %FileCheck -check-prefix NEGATIVE-WITH-DISABLED %s < %t/with-disabled.ll

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=linux-androideabi

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

_ = LinkFramework.IComeFromLinkFramework
UsesSubmodule.useSomethingFromSubmodule()

// CHECK: !llvm.linker.options = !{

// CHECK-DAG: !{{[0-9]+}} = !{!"-lLock"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-lStock"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-lUnderlyingClangLibrary"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Indirect"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"HasSubmodule"}

// CHECK-WITH-SWIFT: !{{[0-9]+}} = !{!"-lSwiftAdapter"}

// CHECK-WITH-DISABLED-DAG: !{!"-framework", !"Barrel"}
// CHECK-WITH-DISABLED-DAG: !{!"-framework", !"Indirect"}
// NEGATIVE-WITH-DISABLED-NOT: !"LinkFramework"
