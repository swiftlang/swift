// RUN: %target-typecheck-verify-swift

protocol AnyPropertyProtocol {
    associatedtype Root = Any
    associatedtype Value = Any
    associatedtype KP: AnyKeyPath
    var key: KP { get }
    var value: Value { get }
}

protocol PartialPropertyProtocol: AnyPropertyProtocol
where KP: PartialKeyPath<Root> {
}

protocol PropertyProtocol: PartialPropertyProtocol
where KP: WritableKeyPath<Root, Value> {
}

extension Dictionary where Value: AnyPropertyProtocol {
    subscript<R, V, P>(key: Key, path: WritableKeyPath<R, V>) -> P? where P: PropertyProtocol, P.Root == R, P.Value == V {
        return self[key] as? P
    }
}
