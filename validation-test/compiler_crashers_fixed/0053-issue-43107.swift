// RUN: %target-swift-frontend %s -emit-ir

// https://github.com/apple/swift/issues/43107

enum Value {
    case IntValue(Int)
}

protocol Storable {
    associatedtype Representation
    
    static var storageKey : String? { get }
    var representation : Representation { get }
}

protocol RawProducable {
    var rawValueForType : Int16 { get }
    init<T: Storable>(value: T) where T.Representation == Self
}

extension Int : Storable {
    static var storageKey : String? { return "int64Value" }
    var representation : Value { return Value.IntValue(self) }
}
