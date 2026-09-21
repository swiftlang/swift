// RUN: %target-swift-emit-silgen-ossa %s

enum CustomError: Error {
    case error
}

struct Test2<T> {
    init(_: T) async throws(CustomError) {}
}

do {
    _ = try await Test2(1)
} catch {
    print("\(error)")
}
