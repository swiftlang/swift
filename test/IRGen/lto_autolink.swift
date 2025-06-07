// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target %target-cpu-apple-macosx10.15 -lto=llvm-full -parse-stdlib -I %t %s | %FileCheck %s --check-prefix CHECK-MACHO
// RUN: %target-swift-emit-ir -target %target-cpu-apple-macosx10.15 -lto=llvm-thin -parse-stdlib -I %t %s | %FileCheck %s --check-prefix CHECK-MACHO

// CHECK-MACHO-DAG: !llvm.linker.options = !{
// CHECK-MACHO-DAG: !{{[0-9]+}} = !{!"-lempty"}

// RUN: %target-swift-frontend -target %target-cpu-unknown-linux-gnu -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-linux-gnu -lto=llvm-full -parse-stdlib -I %t %s | %FileCheck %s --check-prefix CHECK-ELF
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-linux-gnu -lto=llvm-thin -parse-stdlib -I %t %s | %FileCheck %s --check-prefix CHECK-ELF

// CHECK-ELF-DAG: !llvm.dependent-libraries = !{
// CHECK-ELF-DAG: !{{[0-9]+}} = !{!"empty"}

import empty

// Ensure the dependent libraries embedded by clang are merged.


// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.15 -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target %target-cpu-apple-macosx10.15 -lto=llvm-full -parse-stdlib -nostdimport -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %s | %FileCheck %s --check-prefix CHECK-MACHO-MERGE
// RUN: %target-swift-emit-ir -target %target-cpu-apple-macosx10.15 -lto=llvm-thin -parse-stdlib -nostdimport -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %s | %FileCheck %s --check-prefix CHECK-MACHO-MERGE

// CHECK-MACHO-MERGE-DAG: !llvm.linker.options = !{
// CHECK-MACHO-MERGE-DAG: !{{[0-9]+}} = !{!"-lempty"}
// CHECK-MACHO-MERGE-DAG: !{{[0-9]+}} = !{!"-lautolink-module-map-link"}


// RUN: %target-swift-frontend -target %target-cpu-unknown-linux-gnu -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-linux-gnu -lto=llvm-full -parse-stdlib -nostdimport -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %s | %FileCheck %s --check-prefix CHECK-ELF-MERGE
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-linux-gnu -lto=llvm-thin -parse-stdlib -nostdimport -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %s | %FileCheck %s --check-prefix CHECK-ELF-MERGE

// CHECK-ELF-MERGE-DAG: !llvm.dependent-libraries = !{
// CHECK-ELF-MERGE-DAG: !{{[0-9]+}} = !{!"empty"}
// CHECK-ELF-MERGE-DAG: !{{[0-9]+}} = !{!"module"}
// CHECK-ELF-MERGE-DAG: !{{[0-9]+}} = !{!"transitive-module"}
// CHECK-ELF-MERGE-DAG: !{{[0-9]+}} = !{!"autolink-module-map-link"}


#if TEST_CLANG_OPTIONS_MERGE
import AutolinkElfCPragma
import AutolinkModuleMapLink
#endif

// UNSUPPORTED: OS=watchos && (CPU=arm64_32 || CPU=armv7k)
// UNSUPPORTED: CPU=wasm32
