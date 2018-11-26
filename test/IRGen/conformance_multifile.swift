// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s %S/Inputs/conformance_multifile_1.swift | %FileCheck %s

func g<U>(_ f : (E) throws -> (U)) {}

// The extension E: P should not show up in this filel.
// CHECK-NOT: $s21conformance_multifile1EOAA1PAAMc

// CHECK: $s21conformance_multifile1tyyF
func t() {
  g(E2.Filter)
}

// The extension E: P should not show up in this filel.
// CHECK-NOT: $s21conformance_multifile1EOAA1PAAMc
