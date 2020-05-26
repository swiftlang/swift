// RUN: %target-swift-frontend -typecheck %s -verify

public protocol MyBindableObject {}

@propertyWrapper
public struct MyBinding<T> where T : MyBindableObject { // expected-error{{internal initializer 'init(wrappedValue:)' cannot have more restrictive access than its enclosing property wrapper type 'MyBinding' (which is public)}}
    public var wrappedValue: T
}
class BeaconDetector: MyBindableObject {
    @MyBinding var detector = BeaconDetector()
    init() {
        detector.undefined = 45 // expected-error{{value of type 'BeaconDetector' has no member 'undefined'}}
    }
}
