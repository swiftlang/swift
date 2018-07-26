// RUN: %target-swift-emit-silgen -module-name keypaths -primary-file %s %S/Inputs/keypaths_multi_file_b.swift | %FileCheck %s
// RUN: %target-swift-emit-silgen -module-name keypaths %s -primary-file %S/Inputs/keypaths_multi_file_b.swift | %FileCheck --check-prefix=DEFINITION %s

func foo(x: Int) -> KeyPath<A, Int> {
  switch x {
  case 0:
    return \A.x
  default:
    return \A.[0]
  }
  return \A.x
}

// A.x setter
// CHECK-LABEL: sil hidden_external @$S8keypaths1AV1xSivs
// DEFINITION-LABEL: sil hidden @$S8keypaths1AV1xSivs

// A.subscript setter
// CHECK-LABEL: sil hidden_external @$S8keypaths1AVyS2icis
// DEFINITION-LABEL: sil hidden @$S8keypaths1AVyS2icis
