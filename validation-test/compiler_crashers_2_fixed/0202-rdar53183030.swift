// RUN: not %target-swift-frontend -typecheck %s
protocol MyBindableObject {}

@propertyWrapper
struct MyBinding<T> where T : MyBindableObject {
    public var wrappedV: T
    public var wrapperValue: MyBinding<T> {
        return self
    }
    public init(initialValue: T) {
        self.value = initialValue
    }
}
class BeaconDetector: MyBindableObject {
    struct ContentView {
        @MyBinding var detector = BeaconDetector()
        func foo() {
            _ = detector.undefined == 1
        }
    }
}
