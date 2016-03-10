// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

protocol IntegerArithmetic {
  static func uncheckedSubtract(lhs: Self, rhs: Self) -> (Self, Bool)
}

protocol RandomAccessIndex : IntegerArithmetic {
  associatedtype Distance : IntegerArithmetic
  static func uncheckedSubtract(lhs: Self, rhs: Self) -> (Distance, Bool)
}

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtTQQq_F9archetype16ExistentialTuple
// CHECK-SAME:             identifier: [[TT:".+"]])
// archetype.ExistentialTuple <A : RandomAccessIndex, B>(x : A, y : A) -> B
// CHECK: !DISubprogram(name: "ExistentialTuple", linkageName: "_TF9archetype16ExistentialTuple
// CHECK-SAME:          line: [[@LINE+2]]
// CHECK-SAME:          isDefinition: true
func ExistentialTuple<T: RandomAccessIndex>(x: T, y: T) -> T.Distance {
  // (B, Swift.Bool)
  // CHECK: !DILocalVariable(name: "tmp"
  // CHECK-SAME:             line: [[@LINE+2]]
  // CHECK-SAME:             type: ![[TT]]
  var tmp : (T.Distance, Bool) = T.uncheckedSubtract(x, rhs: y)
  return _overflowChecked((tmp.0, tmp.1))
}

