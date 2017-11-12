// RUN: not %target-swift-frontend %s -typecheck

public struct CollectionWrapper<C:RangeReplaceableCollection where C.Index:Comparable> {

	public private(set) var collection : C

	private init(wrappingCollection: C) {
		collection = wrappingCollection
	}
}

// Export Collection API

extension CollectionWrapper : Collection {

	public var startIndex: C.Index { return collection.startIndex }
	public var endIndex  : C.Index { return collection.endIndex   }

	public func makeIterator() -> C.Iterator {
		return collection.makeIterator()
	}

	public subscript (position: C.Index) -> C.Iterator.Element { return collection[position] }

	public subscript (bounds: Range<C.Index>) -> C.SubSequence { return collection[bounds] }

	public func prefix(upTo end: C.Index) -> C.SubSequence { return collection.prefix(upTo: end) }

	public func suffix(from start: C.Index) -> C.SubSequence { return collection.suffix(from: start) }

	public func prefix(through position: C.Index) -> C.SubSequence { return collection.prefix(through: position) }

	public var isEmpty: Bool { return collection.isEmpty }

    public var count: C.IndexDistance { return collection.count }

    public var first: C.Iterator.Element? { return collection.first }

	public func index(after idx: C.Index) -> C.Index { return collection.index(after: idx) }

	public func index(_ idx: C.Index, offsetBy offset: C.IndexDistance, limitedBy limit: C.Index? = nil) -> C.Index {
		return collection.index(idx, offsetBy: offset, limitedBy: limit)
	}
}

// Export RangeReplaceableCollection API

extension CollectionWrapper : RangeReplaceableCollection {

	public init() {
		self.init(wrappingCollection: C())
	}

	public mutating func replaceSubrange<D : Collection where D.Iterator.Element == C.Iterator.Element>(_ subRange: Range<C.Index>, with newElements: D) {
		collection.replaceSubrange(subRange, with: newElements)
	}
}
