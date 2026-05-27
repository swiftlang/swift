// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/48464

public protocol VectorIndex {
    associatedtype Vector8 : Vector where Vector8.Index == Self, Vector8.Element == UInt8
}
public enum VectorIndex1 : VectorIndex {
    case i0
    public typealias Vector8 = Vector1<UInt8>
}
public protocol Vector {
    associatedtype Index: VectorIndex
    associatedtype Element
    init(elementForIndex: (Index) -> Element)
    subscript(index: Index) -> Element { get set }
}
public struct Vector1<Element> : Vector {
    public var e0: Element
    public init(elementForIndex: (VectorIndex1) -> Element) {
        e0 = elementForIndex(.i0)
    }
    public subscript(index: Index) -> Element {
        get { return e0 }
        set { e0 = newValue }
    }
}
extension Vector where Index == VectorIndex1 {
    public init(_ e0: Element) { fatalError() }
}
