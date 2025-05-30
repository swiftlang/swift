// RUN: %target-typecheck-verify-swift

@propertyWrapper
struct Weak<Value> {
    var wrappedValue: Value? { fatalError() }
}

struct WeakStorage<Item: Hashable> {
    @Weak var action: ((Set<Item>) -> Void)??
}

final class Test {
    var storage: WeakStorage<AnyHashable> = .init()

    func test(items: Set<AnyHashable>) {
        storage.action??(items)
    }
}
