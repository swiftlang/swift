// RUN: %target-swift-frontend -emit-ir %s

public protocol LinkedListNode: AnyObject {
    associatedtype T
}

public class LinkedList<N: LinkedListNode> where N.T: Hashable {
    public typealias T = N.T
}

public struct LinkedListIterator<N: LinkedListNode>: IteratorProtocol {
    public mutating func next() -> N.T? {
      return nil
    }
}

extension LinkedList: Sequence {
    public typealias Element = T
    public typealias Iterator = LinkedListIterator<N>
    public __consuming func makeIterator() -> Iterator {
        fatalError()
    }
}
