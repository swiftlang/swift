// RUN: %target-swift-frontend -O %s -disable-llvm-optzns -emit-sil -g -o - | %FileCheck %s

// CHECK: sil shared [noinline] @_TTSg5V14specialization7AddableS0_S_5ProtoS____TF14specialization3sumuRxS_5ProtorFTxx_x
// CHECK-SAME: $@convention(thin) (Addable, Addable) -> Addable {
// CHECK: bb0(%0 : $Addable, %1 : $Addable):
// CHECK:  debug_value %0 : $Addable, let, name "i", argno 1
// CHECK:  debug_value %1 : $Addable, let, name "j", argno 2

public protocol Proto {
  static func +(lhs: Self, rhs: Self) -> Self
}

@inline(never)
public func sum<T : Proto>(_ i : T, _ j : T) -> T {
  let result = i + j
  return result
}

func add(_ x: Int, _ y: Int) -> Int { return x+y }
public struct Addable : Proto {
  let val : Int
  init(_ i : Int) { val = i }
  public static func +(lhs: Addable, rhs: Addable) -> Addable {
    return Addable(add(lhs.val, rhs.val))
  }
}


public func inc(_ i: inout Addable) {
  i = sum(i, Addable(1))
}
