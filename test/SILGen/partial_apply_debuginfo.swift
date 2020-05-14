// RUN: %target-swift-frontend -emit-silgen %S/Inputs/partial_apply_debuginfo_other.swift -primary-file %s -module-name partial_apply_debuginfo -g -Xllvm -sil-print-debuginfo | %FileCheck %s

func doIt() {
  _ = Horse.buck
}

// CHECK-NOT: partial_apply_debuginfo_other.swift
