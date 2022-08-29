// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/54974

protocol AnyPropertyProtocol {
    associatedtype Root = Any
    associatedtype Value = Any
    associatedtype KP: AnyKeyPath
    var key: KP { get }
    var value: Value { get }
}

// CHECK-LABEL: .PartialPropertyProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : AnyPropertyProtocol, Self.[AnyPropertyProtocol]KP : PartialKeyPath<Self.[AnyPropertyProtocol]Root>
protocol PartialPropertyProtocol: AnyPropertyProtocol
where KP: PartialKeyPath<Root> {
}

// CHECK-LABEL: .PropertyProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : PartialPropertyProtocol, Self.[AnyPropertyProtocol]KP : WritableKeyPath<Self.[AnyPropertyProtocol]Root, Self.[AnyPropertyProtocol]Value>
protocol PropertyProtocol: PartialPropertyProtocol
where KP: WritableKeyPath<Root, Value> {
}

extension Dictionary where Value: AnyPropertyProtocol {
    // CHECK-LABEL: .subscript@
    // CHECK-NEXT: Generic signature: <R, V, P where P : PropertyProtocol, P.[AnyPropertyProtocol]Root == R, P.[AnyPropertyProtocol]Value == V>
    subscript<R, V, P>(key: Key, path: WritableKeyPath<R, V>) -> P? where P: PropertyProtocol, P.Root == R, P.Value == V {
        return self[key] as? P
    }
}
