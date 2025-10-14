// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name borrow_accessor_container -enable-experimental-feature BorrowAndMutateAccessors -o %t %S/Inputs/borrow_accessor_container.swift
// RUN: %target-swift-frontend -I %t -emit-sil %s -verify | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

import borrow_accessor_container

func test() {
  let n = 10_000
  var arr = Array(0...n)
  let count = arr.count
  let sum = arr.withUnsafeMutableBufferPointer { ubpointer in
    let container = Container(ubpointer, count)
    var sum = 0
    for i in 0..<container._count {
      sum &+= container[i]
    }
    return sum
  }
  let expectedSum = n * (n + 1) / 2
  assert(sum == expectedSum)
  let mutated_sum = arr.withUnsafeMutableBufferPointer { ubpointer in
    var container = Container(ubpointer, count)
    var sum = 0
    for i in 0..<container._count {
      container[i] &+= 1
      sum += container[i]
    }
    return sum
  }
  let mutatedExpectedSum = (n + 1) * (n + 2) / 2
  assert(mutated_sum == mutatedExpectedSum)
}

// CHECK: sil @$s25borrow_accessor_container9ContainerVAARi_zrlEyxSicib : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @guaranteed Container<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK: sil @$s25borrow_accessor_container9ContainerVAARi_zrlEyxSiciz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @inout Container<τ_0_0>) -> @inout τ_0_0

