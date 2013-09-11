// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// archetype.ExistentialTuple <A : swift.RandomAccessIndex, B>(x : A, y : A) -> B
// CHECK: _T9archetype16ExistentialTupleUSs17RandomAccessIndex___FT1xQ_1yQ__Q0_
func ExistentialTuple<T: RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  // (B, swift.Bool)
  // CHECK: _TtTQ0_Sb_
  var tmp : (T.DistanceType, Bool) = T.sub(x, y)
  alwaysTrap(tmp.1 == false)
  return tmp.0
}

