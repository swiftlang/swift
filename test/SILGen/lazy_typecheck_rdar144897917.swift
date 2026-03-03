// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -enable-library-evolution -module-name Test -experimental-lazy-typecheck | %FileCheck %s

public enum E: CaseIterable {
  case a
  @available(deprecated, renamed: "a") // Intentionally invalid
  case b
}

// CHECK: sil_witness_table E: CaseIterable module Test
