// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/weaklinked_import_helper.swiftmodule -parse-as-library %S/Inputs/weaklinked_import_helper.swift -enable-library-evolution
//
// RUN: echo '@_weakLinked import weaklinked_import_helper' > %t/intermediate.swift
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/intermediate.swiftmodule -parse-as-library %t/intermediate.swift -I %t -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %s -I %t -emit-ir | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

import intermediate
import weaklinked_import_helper

// Symbols from weaklinked_import_helper should be strong, despite intermediate
// importining the module @_weakLinked.

// CHECK-DAG: declare swiftcc {{.+}} @"$s24weaklinked_import_helper2fnyyF"()
fn()
