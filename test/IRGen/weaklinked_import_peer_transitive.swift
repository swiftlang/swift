// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weaklinked_import_helper.swiftmodule -parse-as-library %S/Inputs/weaklinked_import_helper.swift -enable-library-evolution
//
// RUN: echo '@_exported import weaklinked_import_helper' > %t/intermediate.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/intermediate.swiftmodule -parse-as-library %t/intermediate.swift -I %t -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import intermediate

// CHECK-DAG: declare extern_weak swiftcc {{.+}} @"$s24weaklinked_import_helper2fnyyF"()
fn()
