// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: sdk_overlay

import StdlibUnittest
import Foundation
import Dispatch

import SwiftSyntax

var LazyCaching = TestSuite("LazyCaching")

LazyCaching.test("Pathological") {
  let tuple = SyntaxFactory.makeVoidTupleType()

  DispatchQueue.concurrentPerform(iterations: 100) { _ in
    expectEqual(tuple.leftParen, tuple.leftParen)
  }
}

LazyCaching.test("TwoAccesses") {
  let tuple = SyntaxFactory.makeVoidTupleType()

  let queue1 = DispatchQueue(label: "queue1")
  let queue2 = DispatchQueue(label: "queue2")

  var node1: TokenSyntax?
  var node2: TokenSyntax?

  let group = DispatchGroup()
  queue1.async(group: group) {
    node1 = tuple.leftParen
  }
  queue2.async(group: group) {
    node2 = tuple.leftParen
  }
  group.wait()

  let final = tuple.leftParen

  expectNotNil(node1)
  expectNotNil(node2)
  expectEqual(node1, node2)
  expectEqual(node1, final)
  expectEqual(node2, final)
}

runAllTests()
