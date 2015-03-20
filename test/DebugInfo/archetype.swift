// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

protocol IntegerArithmeticType {
  static func uncheckedSubtract(lhs: Self, rhs: Self) -> (Self, Bool)
}

protocol RandomAccessIndexType : IntegerArithmeticType {
  typealias Distance : IntegerArithmeticType
  static func uncheckedSubtract(lhs: Self, rhs: Self) -> (Distance, Bool)
}

// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtTQQq_F9archetype16ExistentialTuple
// CHECK-SAME:             identifier: [[TT:".+"]])
// archetype.ExistentialTuple <A : RandomAccessIndexType, B>(x : A, y : A) -> B
// CHECK: !MDSubprogram(name: "ExistentialTuple", linkageName: "_TF9archetype16ExistentialTuple
// CHECK-SAME:          line: [[@LINE+2]]
// CHECK-SAME:          isDefinition: true
func ExistentialTuple<T: RandomAccessIndexType>(x: T, y: T) -> T.Distance {
  // (B, Swift.Bool)
  // CHECK: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "tmp"
  // CHECK-SAME:             line: [[@LINE+2]]
  // CHECK-SAME:             type: ![[TT]]
  var tmp : (T.Distance, Bool) = T.uncheckedSubtract(x, rhs: y)
  return _overflowChecked((tmp.0, tmp.1))
}

