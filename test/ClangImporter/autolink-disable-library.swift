// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -sdk %S/Inputs -Fsystem %S/Inputs/System/Library/Frameworks -enable-objc-interop -I %S/Inputs/custom-modules -disable-autolink-library Stock -disable-autolink-library UnderlyingClangLibrary -module-name AutolinkDisableFrameworks -emit-ir -o %t/test.ll
// RUN: cat %t/test.ll | %FileCheck %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

// CHECK: !llvm.linker.options = !{

// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lLock"|"/DEFAULTLIB:Lock.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Indirect"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"HasSubmodule"}

// CHECK-NOT: !{!{{"-lStock"|"/DEFAULTLIB:Stock.lib"}}}
// CHECK-NOT: !{!{{"-lUnderlyingClangLibrary"|"/DEFAULTLIB:UnderlyingClangLibrary.lib"}}}
