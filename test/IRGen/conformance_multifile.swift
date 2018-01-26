// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s %S/Inputs/conformance_multifile_1.swift | %FileCheck %s

func g<U>(_ f : (E) throws -> (U)) {}

// CHECK: $S21conformance_multifile1tyyF
func t() {
  g(E2.Filter)
}
