// RUN: not %target-swift-frontend -typecheck %s

@usableFromInline final class Box<Wrapped> {
    init(_ wrapped: Wrapped) { }
}

public protocol List {
    associatedtype Element
    var node: ListNode<Element, Self>? { get }
}

public struct ListNode<Element, Rest: List> where Rest.Element == Element { }

public struct Strict<_Element>: List {
  // N.B. Uncommenting this declaration worked around the original issue.
//    public typealias Element = _Element
    public var node: ListNode<Element, Strict<Element>>? {
        fatalError()
    }

    var box: Box<ListNode<Element, Self>>?
}
