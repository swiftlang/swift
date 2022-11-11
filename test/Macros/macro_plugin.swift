// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -I%platform-module-dir/../.. -L%platform-dylib-dir/../.. -emit-library -emit-library-path=%t/%target-library-name(MacroDefinition) -working-directory=%t -module-name=MacroDefinition %S/Inputs/macro_definition.swift
// RUN: %target-swift-frontend -L%platform-dylib-dir/../.. -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -disable-availability-checking -dump-ast -primary-file %s | %FileCheck %s

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// rdar://102160067
// UNSUPPORTED: CPU=arm64e

let _ = #customStringify(1.byteSwapped + 2.advanced(by: 10))

// CHECK: (macro_expansion_expr type='(Int, String)' {{.*}} name=customStringify
// CHECK:   (argument_list
// EXPANSION BEGINS
// CHECK:   (tuple_expr type='(Int, String)'
// CHECK:     (binary_expr type='Int'
// CHECK:     (string_literal_expr type='String'

let _ = #customStringify(1.0.truncatingRemainder(dividingBy: 1.0) + 3.0)

// CHECK: (macro_expansion_expr type='(Double, String)' {{.*}} name=customStringify
// CHECK:   (argument_list
// EXPANSION BEGINS
// CHECK:   (tuple_expr type='(Double, String)'
// CHECK:     (binary_expr type='Double'
// CHECK:     (string_literal_expr type='String'

let _ = #customStringify(["a", "b", "c"] + ["d", "e", "f"])

// CHECK: (macro_expansion_expr type='([String], String)' {{.*}} name=customStringify
// CHECK:   (argument_list
// EXPANSION BEGINS
// CHECK:   (tuple_expr type='([String], String)'
// CHECK:     (binary_expr type='[String]'
// CHECK:     (string_literal_expr type='String'
