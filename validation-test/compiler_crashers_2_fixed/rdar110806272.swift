// RUN: not %target-swift-frontend -typecheck %s

struct SyntaxCollectionIterator<E: SyntaxProtocol>: IteratorProtocol {
  typealias Element = E
}

protocol SyntaxCollection: BidirectionalCollection {}

extension SyntaxCollection {
  typealias Iterator = SyntaxCollectionIterator<Element>
}

struct AccessorListSyntax: SyntaxCollection {
  typealias Element = Int
}
