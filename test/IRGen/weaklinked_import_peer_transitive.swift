// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weaklinked_import_helper.swiftmodule -parse-as-library %S/Inputs/weaklinked_import_helper.swift -enable-library-evolution
//
// RUN: echo '@_exported import weaklinked_import_helper' > %t/intermediate.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/intermediate.swiftmodule -parse-as-library %t/intermediate.swift -I %t -enable-library-evolution
//
// RUN: echo '@_exported import weaklinked_import_helper_clang' > %t/intermediate_clang.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/intermediate_clang.swiftmodule -parse-as-library %t/intermediate_clang.swift -I %t -enable-library-evolution -Xcc -fmodule-map-file=%S/Inputs/weaklinked_import_helper_clang.modulemap
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir -Xcc -fmodule-map-file=%S/Inputs/weaklinked_import_helper_clang.modulemap | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import intermediate
@_weakLinked import intermediate_clang

// CHECK-DAG: declare extern_weak swiftcc {{.+}} @"$s24weaklinked_import_helper2fnyyF"()
fn()
// CHECK-DAG: declare extern_weak void @clang_fn()
clang_fn()
