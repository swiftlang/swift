// RUN: %target-swift-frontend -emit-silgen %s

// Reduced from swift-nio.

public protocol AppendableCollection: Collection {
    mutating func append(_ newElement: Self.Iterator.Element)
}

public struct CircularBuffer<E>: AppendableCollection {
    public mutating func append(_ value: E) {
        fatalError()
    }

    private mutating func doubleCapacity() {
        fatalError()
    }

    public subscript(index: Int) -> E {
        get {
            fatalError()
        }
        set {
            fatalError()
        }
    }

    public var indices: Range<Int> {
        fatalError()
    }

    public var isEmpty: Bool {
        fatalError()
    }

    public var count: Int {
        fatalError()
    }

    public var startIndex: Int {
        fatalError()
    }

    public var endIndex: Int {
        fatalError()
    }

    public func index(after: Int) -> Int {
        fatalError()
    }

    public func index(before: Int) -> Int {
        fatalError()
    }
}

