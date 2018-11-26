// RUN: not %swift -typecheck -target %target-triple -primary-file %s  -emit-fixits-path %t.main.remap -primary-file %S/Inputs/batch-mode-helper.swift -emit-fixits-path %t.helper.remap -diagnostics-editor-mode
// RUN: %FileCheck -check-prefix=CHECK-MAIN %s < %t.main.remap
// RUN: %FileCheck -check-prefix=NEGATIVE-MAIN %s < %t.main.remap
// RUN: %FileCheck -check-prefix=CHECK-HELPER %s < %t.helper.remap
// RUN: %FileCheck -check-prefix=NEGATIVE-HELPER %s < %t.helper.remap

// CHECK-MAIN: "file": "{{.+}}batch-mode.swift"
// CHECK-MAIN: "text": "case .a:\n<#code#>\ncase .b:\n<#code#>\ncase .c:\n<#code#>\n"
// NEGATIVE-MAIN-NOT: batch-mode-helper.swift

// CHECK-HELPER: "file": "{{.+}}batch-mode-helper.swift"
// CHECK-HELPER: "text": "case .x:\n<#code#>\ncase .y:\n<#code#>\ncase .z:\n<#code#>\n"
// NEGATIVE-HELPER-NOT: batch-mode.swift

enum E1 {
  case a
  case b
  case c
}

func fooMain(_ e: E1) {
  switch e {
  }
}
