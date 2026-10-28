// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-6.2-abi-triple %s -emit-module -enable-experimental-feature RawLayout -parse-as-library -o %t
// RUN: %target-sil-opt -target %target-swift-6.2-abi-triple -enable-sil-verify-all %t/value_generics.swiftmodule -o - | %FileCheck %s

// REQUIRES: swift_feature_RawLayout

// CHECK: @_rawLayout(likeArrayOf: Element, count: Count) struct Vector<Element, let Count : Int> : ~Copyable where Element : ~Copyable {
@_rawLayout(likeArrayOf: Element, count: Count)
struct Vector<Element: ~Copyable, let Count: Int>: ~Copyable {}

// CHECK: extension Vector where Element == Int
extension Vector where Element == Int {
  func something() {}
}

// CHECK: extension Vector where Count == 2
extension Vector where Count == 2 {
  func something2() {}
}

// CHECK: func something<let N : Int>(_: borrowing Vector<Int, N>)
func something<let N: Int>(_: borrowing Vector<Int, N>) {}

// CHECK: func hello(_: [4 of Int])
func hello(_: [4 of Int]) {}
