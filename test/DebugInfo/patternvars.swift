// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

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
// sure we emit a dbg.value once per basic block.

// CHECK: define {{.*}}@"$s11patternvars6mangle1sSayAA13UnicodeScalarVGAF_tFA2EXEfU_"
// CHECK: %[[VAL:[0-9]+]] = call swiftcc i32 @"$s11patternvars13UnicodeScalarV5values6UInt32Vvg"(i32 %0)
// CHECK-NEXT:  call void @llvm.dbg.value(metadata i32 %[[VAL]]
// CHECK:       ; <label>
// CHECK:       call void @llvm.dbg.value(metadata i32 %[[VAL]]
// CHECK-NOT:   call void @llvm.dbg.value
// CHECK-NOT:   call void asm sideeffect "", "r"

// CHECK:       ; <label>
// CHECK:       call void @llvm.dbg.value(metadata i32 %[[VAL]]
// CHECK-NOT:   call void @llvm.dbg.value
// CHECK-NOT:   call void asm sideeffect "", "r"

// CHECK:       ; <label>
// CHECK:       call void @llvm.dbg.value(metadata i32 %[[VAL]]
// CHECK-NOT:   call void @llvm.dbg.value
// CHECK-NOT:   call void asm sideeffect "", "r"

// CHECK:       ; <label>
// CHECK:       call void @llvm.dbg.value(metadata i32 %[[VAL]]
// CHECK-NOT:   call void @llvm.dbg.value
// CHECK-NOT:   call void asm sideeffect "", "r"

