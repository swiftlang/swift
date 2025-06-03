// RUN: not %target-swift-frontend -typecheck

enum E: Error, Equatable {
    case error1
}

public struct S {}

public struct S2: AsyncSequence {
    public typealias Element = S
    public init() {}
    public func makeAsyncIterator() -> AsyncIterator { return AsyncIterator() }
    public struct AsyncIterator: AsyncIteratorProtocol {
        public func next() async throws -> S? {
            return nil
        }
    }
}

public struct S3: Sendable {
    public func f() async throws -> AsyncSequence<S, E> {
        return S2()
    }
}

