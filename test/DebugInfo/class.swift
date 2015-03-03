// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// FIXME: This is more of a crash test than anything else.
// CHECK: !MDCompositeType(tag: DW_TAG_structure_type, name: "Tree",{{.*}} line: [[@LINE+1]]
class Tree<T> {
  var l, r: Tree<T>?
}

var T : Tree<Int>
