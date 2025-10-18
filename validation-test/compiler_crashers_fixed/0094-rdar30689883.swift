// RUN: not %target-swift-frontend %s -emit-ir

public struct Trie<Key: Hashable, Value> {

    internal var root: TrieNode<Key, Value>

    public init() {
        self.root = TrieNode<Key, Value>()
    }

    public mutating func removeValue<C: Collection>(forCollection collection: C) -> Value? where C.Iterator.Element == Key {
        return self.root.removeValue(forIterator: collection.makeIterator())
    }

    public mutating func updateValue<C: Collection>(_ value: Value, forCollection collection: C) -> Value? where C.Iterator.Element == Key {
        return self.root.updateValue(value, forIterator: collection.makeIterator())
    }

    public func value<C: Collection>(forCollection collection: C) -> Value? where C.Iterator.Element == Key {
        return self.root.value(forIterator: collection.makeIterator())
    }
}

internal struct TrieNode<Key: Hashable, Value> {

    internal let children: Dictionary<Key, TrieNode<Key, Value>>

    internal let value: Value?

    internal init(value: Value?=nil) {
        self.children = Dictionary<Key, TrieNode<Key, Value>>()
        self.value = value
    }

    internal mutating func removeValue<I: IteratorProtocol>(forIterator iterator: inout I) -> Value? where I.Element == Key {
        return nil
    }

    internal mutating func updateValue<I: IteratorProtocol>(_ value: Value, forIterator iterator: inout I) -> Value? where I.Element == Key {
        return nil
    }

    internal func value<I: IteratorProtocol>(forIterator iterator: inout I) -> Value? where I.Element == Key {
        guard let key = iterator.next() else {
            return value
        }

        return self.children[key]?.value(forIterator: iterator)
    }
}


