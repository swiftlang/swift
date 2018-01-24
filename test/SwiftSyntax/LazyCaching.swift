// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import Dispatch

import SwiftSyntax

var LazyCaching = TestSuite("LazyCaching")

LazyCaching.test("Pathological") {
  let tuple = SyntaxFactory.makeVoidTupleType()

  DispatchQueue.concurrentPerform(iterations: 100) { _ in
    expectEqual(tuple.leftParen.uniqueIdentifier, 
                tuple.leftParen.uniqueIdentifier)
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
  expectEqual(node1?.uniqueIdentifier, node2?.uniqueIdentifier)
  expectEqual(node1?.uniqueIdentifier, final.uniqueIdentifier)
  expectEqual(node2?.uniqueIdentifier, final.uniqueIdentifier)
}

runAllTests()
