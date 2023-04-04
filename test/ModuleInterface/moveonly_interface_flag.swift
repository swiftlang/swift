// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -I %t
// RUN: %FileCheck %s < %t/Library.swiftinterface

// this test makes sure that decls containing a move-only type are guarded by the $MoveOnly feature flag

// CHECK:       #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:    public struct MoveOnlyStruct : ~Swift.Copyable {

// CHECK:       #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:    @_moveOnly public struct OldAttr_MoveOnlyStruct {

// CHECK:      #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:   public enum MoveOnlyEnum : ~Swift.Copyable {

// CHECK:      #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:   public func someFn() -> Library.MoveOnlyEnum

// CHECK:     public class What {
// CHECK:       #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:    public func diamonds(_ f: (borrowing Library.MoveOnlyStruct) -> Swift.Int)

// CHECK: #if compiler(>=5.3) && $MoveOnly
// CHECK-NEXT:  extension Library.MoveOnlyStruct {

public struct MoveOnlyStruct : ~Copyable {
  let x = 0
}

@_moveOnly public struct OldAttr_MoveOnlyStruct {
  let x = 0
}

public enum MoveOnlyEnum : ~Copyable {
  case depth
}

public func someFn() -> MoveOnlyEnum { return .depth }

public class What {
  public func diamonds(_ f: (borrowing MoveOnlyStruct) -> Int) {}
}

public extension MoveOnlyStruct {
  func who() {}
}
