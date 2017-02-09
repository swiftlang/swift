// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

@_fixed_layout
public struct UnicodeScalar {
  var _value: UInt32
  public var value: UInt32 { return _value }
}

public func mangle(s: [UnicodeScalar]) -> [UnicodeScalar] {
  let replacementUnichar = UnicodeScalar(_value: 0)
  var mangledUnichars: [UnicodeScalar] = s.map {
    switch $0.value {
      case
      // A-Z
      0x0041...0x005A,
      // a-z
      0x0061...0x007A,
      // 0-9
      0x0030...0x0039,
      // _
      0x005F,
      // Latin (1)
      0x00AA...0x00AA:
      return $0
    default:
      return replacementUnichar
    }
  }
  return mangledUnichars
}

// The patterns in the first case statement each define an anonymous variable,
// which shares the storage with the expression in the switch statement. Make
// sure we only emit live range extensions for the storage once per basic block.

// CHECK: define {{.*}}@_T011patternvars6mangleSayAA13UnicodeScalarVGSayADG1s_tFAdDcfU_
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
// CHECK: call void asm sideeffect "", "r"
// CHECK-NOT: call void asm sideeffect "", "r"
// CHECK: br {{.*}}label
