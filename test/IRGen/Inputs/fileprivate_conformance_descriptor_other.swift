public class Outer {
    public struct Inner: _Private {
        public init() {}
    }
}

private protocol _Private: AsyncSequence where Element == Int {
}

extension _Private {
    public func makeAsyncIterator() -> AsyncStream<Int>.AsyncIterator {
        AsyncStream<Int> { nil }.makeAsyncIterator()
    }
}
