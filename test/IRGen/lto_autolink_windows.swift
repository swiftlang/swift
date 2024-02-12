// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target x86_64-unknown-windows-msvc -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target x86_64-unknown-windows-msvc -lto=llvm-full -parse-stdlib -I %t %S/lto_autolink.swift | %FileCheck %s --check-prefix CHECK-COFF
// RUN: %target-swift-emit-ir -target x86_64-unknown-windows-msvc -lto=llvm-thin -parse-stdlib -I %t %S/lto_autolink.swift | %FileCheck %s --check-prefix CHECK-COFF

// REQUIRES: OS=windows-msvc

// CHECK-COFF-DAG: !llvm.linker.options = !{
// CHECK-COFF-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:empty.lib"}

// RUN: %target-swift-frontend -target %target-cpu-unknown-windows-msvc -emit-module -parse-stdlib -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-windows-msvc -lto=llvm-full -parse-stdlib -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %S/lto_autolink.swift | %FileCheck %s --check-prefix CHECK-COFF
// RUN: %target-swift-emit-ir -target %target-cpu-unknown-windows-msvc -lto=llvm-thin -parse-stdlib -I %t -I %S/Inputs -DTEST_CLANG_OPTIONS_MERGE %S/lto_autolink.swift | %FileCheck %s --check-prefix CHECK-COFF

// CHECK-COFF-MERGE-DAG: !llvm.linker.options = !{
// CHECK-COFF-MERGE-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:empty.lib"}
// CHECK-COFF-MERGE-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:module.lib"}
// CHECK-COFF-MERGE-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:transitive-module.lib"}
// CHECK-COFF-MERGE-DAG: !{{[0-9]+}} = !{!"/DEFAULTLIB:autolink-module-map-link.lib"}
