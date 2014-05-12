// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

protocol IntegerArithmetic {
  class func uncheckedSubtract(lhs: Self, rhs: Self) -> (Self, Bool)
}

protocol RandomAccessIndex : IntegerArithmetic {
  typealias DistanceType : IntegerArithmetic
  class func uncheckedSubtract(lhs: Self, rhs: Self) -> (DistanceType, Bool)
}

// CHECK: null, null, metadata ![[TT:.*]]} ; [ DW_TAG_structure_type ] [_TtTQQq_F9archetype16ExistentialTuple{{.*}}]
// archetype.ExistentialTuple <A : RandomAccessIndex, B>(x : A, y : A) -> B
// CHECK: _TF9archetype16ExistentialTuple{{.*}} [ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] [ExistentialTuple]
func ExistentialTuple<T: RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  // (B, Swift.Bool)
  // CHECK: metadata !"tmp", metadata {{.*}}, i32 [[@LINE+1]], metadata ![[TT]], i32 0, i32 0} ; [ DW_TAG_auto_variable ] [tmp]
  var tmp : (T.DistanceType, Bool) = T.uncheckedSubtract(x, rhs: y)
  return overflowChecked((tmp.0, tmp.1))
}

