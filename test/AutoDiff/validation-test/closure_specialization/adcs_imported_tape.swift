// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ADCSLibrary)) %S/Inputs/adcs_imported_tape_library.swift -O -emit-module -emit-module-path %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-build-swift %s %S/Inputs/adcs_imported_tape_other.swift -parse-as-library -I %t -L %t -lADCSLibrary -O -Xfrontend -sil-verify-all -o %t/client %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(ADCSLibrary)
// RUN: %target-codesign %t/client
// RUN: %target-run %t/client %t/%target-library-name(ADCSLibrary)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ADCSLibrary)) %S/Inputs/adcs_imported_tape_library.swift -O -enable-library-evolution -emit-module -emit-module-path %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-build-swift %s %S/Inputs/adcs_imported_tape_other.swift -parse-as-library -I %t -L %t -lADCSLibrary -O -enable-library-evolution -Xfrontend -sil-verify-all -o %t/resilient-client %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(ADCSLibrary)
// RUN: %target-codesign %t/resilient-client
// RUN: %target-run %t/resilient-client %t/%target-library-name(ADCSLibrary)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -enable-library-evolution -parse-as-library -module-name ADCSClient -I %t %S/Inputs/adcs_imported_tape_other.swift -primary-file %s -o %t/client.sil
// RUN: %FileCheck %s < %t/client.sil

import _Differentiation

import ADCSLibrary

// CHECK-LABEL: {{^}}// specialized pullback of clientInt(_:_:)
// CHECK:       bb0(%[[#]] : $Float, %[[#INT_TAPE:]] : $[[INT_ENUM:_AD__[^ )]*_spec[^ )]*]]):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#INT_TAPE]], case #[[INT_ENUM]].
// CHECK:       bb[[#]](%[[#]] : $(predecessor: {{_AD__[^,]+}}<Int>, (Float, Float))):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function

// CHECK-LABEL: {{^}}// specialized pullback of clientDouble(_:_:)
// CHECK:       bb0(%[[#]] : $Float, %[[#DOUBLE_TAPE:]] : $[[DOUBLE_ENUM:_AD__[^ )]*_spec[^ )]*]]):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:         switch_enum %[[#DOUBLE_TAPE]], case #[[DOUBLE_ENUM]].
// CHECK:       bb[[#]](%[[#]] : $(predecessor: {{_AD__[^,]+}}<Double>, (Float, Float))):
// CHECK-NOT:     = apply {{.*}} : $@callee_guaranteed
// CHECK:       } // end sil function
@differentiable(reverse, wrt: x)
public func clientInt(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

@differentiable(reverse, wrt: x)
public func clientDouble(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Double(1), flag)
}

// Both client primary files specialize importedBranch<Int>, so their
// synthesized enum metadata must link.
@main
struct ImportedTapeTests {
  static func main() {
    for flag in [true, false] {
      let integer = valueWithPullback(at: Float(3), of: { clientInt($0, flag) })
      let floating = valueWithPullback(at: Float(3), of: { clientDouble($0, flag) })
      let other = valueWithPullback(at: Float(3), of: { clientOther($0, flag) })
      precondition(integer.value == (flag ? 9 : 6))
      precondition(floating.value == (flag ? 9 : 6))
      precondition(other.value == (flag ? 9 : 6))
      for seed in [Float(1), 5, 2] {
        let expected = (flag ? 6 : 2) * seed
        precondition(integer.pullback(seed) == expected)
        precondition(floating.pullback(seed) == expected)
        precondition(other.pullback(seed) == expected)
      }
    }
  }
}
