// RUN: %empty-directory(%t)

// RUN: %target-build-swift -module-name keypaths_resilient -emit-module %S/Inputs/keypaths_multi_file_c.swift -emit-module-path %t/keypaths_resilient.swiftmodule

// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -module-name keypaths -language-mode 4 -primary-file %s %S/Inputs/keypaths_multi_file_b.swift -I %t
// RUN: %target-swift-emit-silgen -module-name keypaths -primary-file %s %S/Inputs/keypaths_multi_file_b.swift -I %t -language-mode 4 | %FileCheck %s
// RUN: %target-swift-emit-silgen -module-name keypaths %s -primary-file %S/Inputs/keypaths_multi_file_b.swift -I %t | %FileCheck --check-prefix=DEFINITION %s
// RUN: %target-swift-emit-silgen -module-name keypaths -primary-file %s %S/Inputs/keypaths_multi_file_b.swift -I %t | %FileCheck --check-prefix=SWIFT5 %s

import keypaths_resilient

func foo(x: Int) -> KeyPath<A, Int> {
  switch x {
  case 0:
    return \A.x
  default:
    return \A.[0]
  }
  return \A.x
}

// In Swift 5, a key path to a private(set) member referenced from outside its
// file is read-only, so it has no setter component and no setter reference.
// SWIFT5-LABEL: sil hidden [ossa] @$s8keypaths3foo1xs7KeyPathCyAA1AVSiGSi_tF
// SWIFT5: keypath $KeyPath<A, Int>, (root $A; gettable_property $Int, id @$s8keypaths1AV1xSivg
// SWIFT5: keypath $KeyPath<A, Int>, (root $A; gettable_property $Int, id @$s8keypaths1AVyS2icig
// SWIFT5-NOT: @$s8keypaths1AV1xSivpACTk
// SWIFT5-NOT: @$s8keypaths1AV1xSivs
// SWIFT5-NOT: @$s8keypaths1AVyS2icipACTk
// SWIFT5-NOT: @$s8keypaths1AVyS2icis

// A.x setter
// CHECK-LABEL: sil hidden_external @$s8keypaths1AV1xSivs
// DEFINITION-LABEL: sil hidden [ossa] @$s8keypaths1AV1xSivs

// A.subscript setter
// CHECK-LABEL: sil hidden_external @$s8keypaths1AVyS2icis
// DEFINITION-LABEL: sil hidden [ossa] @$s8keypaths1AVyS2icis

func bar<T>(_: T) {
  _ = \C<T>.b
  _ = \C<T>[0]

  _ = \D<T>.b
  _ = \D<T>[0]

  _ = \P.b

  // FIXME: crashes
  // _ = \P[0]
}

// https://github.com/apple/swift/issues/51158
class MM<T: P> : PP {}
func foo<T3: BB, T4: MM<T3>>(t: T3, u: T4) {
  let _ = \CC<T4>.x
}
