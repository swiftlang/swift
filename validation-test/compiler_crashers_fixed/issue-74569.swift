// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

protocol Cache<Object> {
    associatedtype Object
    associatedtype Snapshot: Sequence<Object>

    func entries(_ body: (Snapshot) -> Void) -> AsyncStream<Object>
}

func readFromCacheImpl<T>(cache: any Cache<T>) async {
    let updateStream = cache.entries { _ in }

    for await _ in updateStream {}
}
