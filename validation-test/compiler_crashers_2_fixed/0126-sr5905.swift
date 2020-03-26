// RUN: %target-typecheck-verify-swift

protocol VectorIndex {
    associatedtype Vector8 : Vector where Vector8.Index == Self, Vector8.Element == UInt8
}
enum VectorIndex1 : VectorIndex {
    case i0
    typealias Vector8 = Vector1<UInt8>
}
protocol Vector {
    associatedtype Index: VectorIndex
    associatedtype Element
    init(elementForIndex: (Index) -> Element)
    subscript(index: Index) -> Element { get set }
}
struct Vector1<Element> : Vector {
    //typealias Index = VectorIndex1 // Uncomment this line to workaround bug.
    var e0: Element
    init(elementForIndex: (VectorIndex1) -> Element) {
        e0 = elementForIndex(.i0)
    }
    subscript(index: Index) -> Element { // expected-error {{reference to invalid associated type 'Index' of type 'Vector1<Element>'}}
        get { return e0 }
        set { e0 = newValue }
    }
}
extension Vector where Index == VectorIndex1 {
    init(_ e0: Element) { fatalError() }
}
