// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

import Foundation

public struct CustomCollection<Element>: RandomAccessCollection, MutableCollection, ExpressibleByArrayLiteral {
    public typealias Indices = Range<Int>
    public typealias Index = Int

    public init() {}

    public init(arrayLiteral elements: Element ...) {
        fatalError()
    }
}

extension CustomCollection {
    public var startIndex: Int { fatalError() }
    public var endIndex: Int { fatalError() }

    public subscript(position: Int) -> Element {
        get { fatalError() }
        set { fatalError() }
    }
}

extension CustomCollection: RangeReplaceableCollection {
    public mutating func append(_ newElement: Element) { }
    public mutating func append<S: Sequence>(contentsOf newElements: S) where S.Element == Element { }
    public mutating func reserveCapacity(_ minimumCapacity: Int) { }
    public mutating func removeAll(keepingCapacity keepCapacity: Bool = false) { }
    public mutating func replaceSubrange<C: Collection>(_ subRange: Range<Int>, with newElements: C) where C.Element == Element { }
}

extension CustomCollection {
    public func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R { fatalError() }
}

extension CustomCollection: ContiguousBytes where Element == UInt8 { }

extension CustomCollection: DataProtocol where Element == UInt8 {
    public var regions: CollectionOfOne<CustomCollection<UInt8>> { fatalError() }
}
