// Natural numbers encoded as types:
protocol NaturalNumberType { static var intValue: Int { get } }
struct NaturalNumberZero : NaturalNumberType {
    static var intValue: Int { return 0 }
}
struct NaturalNumberSuccessorOf<T: NaturalNumberType> : NaturalNumberType {
    static var intValue: Int { return T.intValue + 1 }
}
// See FourFloats below for an example of how to use the following:
protocol StaticStorageType {
    associatedtype Element
    associatedtype Count: NaturalNumberType
}
extension StaticStorageType {
    typealias Successor = StaticStorageSuccessorOf<Self>
}
struct StaticStorageOfOne<E> : StaticStorageType {
    typealias Element = E
    typealias Count = NaturalNumberZero
    var storage: Element
}
struct StaticStorageSuccessorOf<T: StaticStorageType> : StaticStorageType {
    typealias Element = T.Element
    typealias Count = NaturalNumberSuccessorOf<T.Count>
    var storage: (Element, T)
}
// Using the StaticStorage-thing:
typealias FourFloats = StaticStorageOfOne<Float>.Successor.Successor.Successor

//-----------------------------------------------------------------------------
// This function works and will print the number of bytes of FourFloats, ie 16.
// But compiler will crash if an identical function is declared in main.swift.
//-----------------------------------------------------------------------------
func definedInOther() { print(MemoryLayout<FourFloats>.size) }
