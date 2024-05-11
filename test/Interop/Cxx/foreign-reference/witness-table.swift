// RUN: %target-run-simple-swift(-I %S/Inputs/ -Xfrontend -enable-experimental-cxx-interop -Xfrontend -validate-tbd-against-ir=none -Xfrontend -disable-llvm-verify -Xfrontend -disable-availability-checking -g)
//
// REQUIRES: executable_test
// XFAIL: OS=windows-msvc

import StdlibUnittest
import WitnessTable

public protocol ListNode {
  associatedtype Element
  func next() -> Element?
}

public struct List<NodeType: ListNode> : Sequence, IteratorProtocol
      where NodeType.Element == NodeType {
  private var currentNode: NodeType?

  public init(startingAt start: NodeType?) { currentNode = start }

  public mutating func next() -> NodeType? {
    if let node = currentNode {
      currentNode = node.next()
      return node
    }
    return nil
  }
}

extension CxxLinkedList : ListNode { }

var WitnessTableTestSuite = TestSuite("Use foreign reference in a generic context")

WitnessTableTestSuite.test("As generic argument to List") {
  let first = makeLinkedList()
  let list = List(startingAt: first)
  var count = 0
  for e in list {
    expectEqual(count, Int(e.value))
    count += 1
  }
  expectEqual(count, 4)
}

extension CxxSequence : Sequence, IteratorProtocol { }

WitnessTableTestSuite.test("As a Sequence") {
  let list = makeSequence()
  var count = 0
  for e in list {
    expectEqual(count, Int(e.value))
    count += 1
  }
  expectEqual(count, 3)
}

runAllTests()

