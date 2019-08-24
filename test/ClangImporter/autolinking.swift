// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -sdk %S/Inputs -Fsystem %S/Inputs/System/Library/Frameworks -enable-objc-interop -I %S/Inputs/custom-modules -emit-ir -o - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module %S/Inputs/overlay.swift -sdk %S/Inputs -module-link-name SwiftOverlay -module-name ClangModuleWithOverlay -I %S/Inputs/custom-modules -o %t

// RUN: %target-swift-frontend %s -sdk %S/Inputs -Fsystem %S/Inputs/System/Library/Frameworks -enable-objc-interop -I %S/Inputs/custom-modules -I %t -emit-ir -o %t/with-adapter.ll
// RUN: %FileCheck %s < %t/with-adapter.ll
// RUN: %FileCheck -check-prefix CHECK-WITH-SWIFT %s < %t/with-adapter.ll

// RUN: %target-swift-frontend %s -sdk %S/Inputs -I %S/Inputs/custom-modules -Fsystem %S/Inputs/System/Library/Frameworks -enable-objc-interop -emit-ir -disable-autolink-framework LinkFramework -o - > %t/with-disabled.ll
// RUN: %FileCheck -check-prefix CHECK-WITH-DISABLED %s < %t/with-disabled.ll
// RUN: %FileCheck -check-prefix NEGATIVE-WITH-DISABLED %s < %t/with-disabled.ll

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract

import LinkMusket
import LinkFramework
import ClangModuleUser
import IndirectFrameworkImporter
import UsesSubmodule

_ = LinkFramework.IComeFromLinkFramework
UsesSubmodule.useSomethingFromSubmodule()

// CHECK: !llvm.linker.options = !{

// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lLock"|"/DEFAULTLIB:Lock.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lStock"|"/DEFAULTLIB:Stock.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Barrel"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lUnderlyingClangLibrary"|"/DEFAULTLIB:UnderlyingClangLibrary.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"Indirect"}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"HasSubmodule"}

// CHECK-WITH-SWIFT: !{{[0-9]+}} = !{!{{"-lSwiftOverlay"|"/DEFAULTLIB:SwiftOverlay.lib"}}}

// CHECK-WITH-DISABLED-DAG: !{!"-framework", !"Barrel"}
// CHECK-WITH-DISABLED-DAG: !{!"-framework", !"Indirect"}
// NEGATIVE-WITH-DISABLED-NOT: !"LinkFramework"
