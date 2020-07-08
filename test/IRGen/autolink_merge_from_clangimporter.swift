// This checks that link entries derived from clang importer are merged properly

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name empty -module-link-name empty %S/../Inputs/empty.swift
// RUN: %target-swift-emit-ir -I %t -Fsystem %S/Inputs/Frameworks %s | %FileCheck %s --check-prefix CHECK
// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: autolink-extract

import empty
import LinkFramework

func useLibrarySym() {
  let _ = LinkFramework.IComeFromLinkFramework
}

// CHECK-DAG: !llvm.linker.options = !{
// CHECK-DAG: !{{[0-9]+}} = !{!{{"-lempty"|"/DEFAULTLIB:empty.lib"}}}
// CHECK-DAG: !{{[0-9]+}} = !{!"-framework", !"LinkFramework"}
