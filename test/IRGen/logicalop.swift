// RUN: %swift %s -emit-llvm -o - | FileCheck %s

// Note that "||" has a special case in IRGen; make sure IRGen is sane.
class Node {
  var id : Int
  var name : String
}
func f0(a : String) -> Bool
func f1(dependency : Node) {
  var a = true || f0(dependency.name)
}
// CHECK: define %_T9logicalop4Node* @_TC9logicalop4Node11constructorFT_S0_
// CHECK: define internal i64 @_TL9logicalop4Node10destructor
// CHECK: define void @_T9logicalop2f1FT10dependencyCS_4Node_T_
