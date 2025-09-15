// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=default %s | %FileCheck %s -check-prefix=CHECK-before-fix
// RUN: %target-swift-emit-irgen -I %S/Inputs -cxx-interoperability-mode=default %s -Xcc -DAFTER_FIX | %FileCheck %s -check-prefix=CHECK-after-fix

// REQUIRES: OS=windows-msvc && CPU=aarch64

import SRetWinARM64

public struct OptionalTypeArray {
  private let bridged: BridgedTypeArray
  public init(bridged: BridgedTypeArray) {
    self.bridged = bridged
  }
}

public struct SubstitutionMap {
  public let bridged: BridgedSubstitutionMap

  public var replacementTypes: OptionalTypeArray {
    let types = BridgedTypeArray.fromReplacementTypes(bridged)
    return OptionalTypeArray(bridged: types)
  }
}

public func test(sm: SubstitutionMap) -> OptionalTypeArray {
  return sm.replacementTypes
}

// Check that BridgedTypeArray is indirectly returned via sret after the fix

// CHECK-before-fix: declare {{.*}} [2 x i64] @"?fromReplacementTypes@BridgedTypeArray@@SA?AU1@UBridgedSubstitutionMap@@@Z"(i64)
// CHECK-after-fix: declare {{.*}} void @"?fromReplacementTypes@BridgedTypeArray@@SA?AU1@UBridgedSubstitutionMap@@@Z"(ptr inreg sret(%struct.BridgedTypeArray) align 8, i64)
