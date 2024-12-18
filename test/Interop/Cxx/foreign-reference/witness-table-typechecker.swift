// RUN: %target-typecheck-verify-swift -cxx-interoperability-mode=default -disable-availability-checking -I %S/Inputs

import WitnessTable

public protocol ListNode {
  associatedtype Element
  func next() -> Element?
}

@available(SwiftStdlib 5.8, *)
extension CxxLinkedList : ListNode { }

let existential: any ListNode = makeLinkedList()
let cast: CxxLinkedList? = existential as? CxxLinkedList
