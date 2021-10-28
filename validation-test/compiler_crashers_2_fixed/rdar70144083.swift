// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-concurrency

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
public protocol AsyncIteratorProtocol {
    associatedtype Element
    associatedtype Failure: Error

    mutating func nextResult() async -> Result<Element, Failure>?
    mutating func cancel()
}

@available(SwiftStdlib 5.1, *)
public protocol AsyncSequence {
    associatedtype Element
    associatedtype Failure: Error
    associatedtype AsyncIterator: AsyncIteratorProtocol where AsyncIterator.Element == Element, AsyncIterator.Failure == Failure
    
    func makeAsyncIterator() -> AsyncIterator
}

@available(SwiftStdlib 5.1, *)
struct Just<Element>: AsyncSequence {
    typealias Failure = Never

    struct AsyncIterator: AsyncIteratorProtocol {
        var value: Element?
        mutating func nextResult() async -> Result<Element, Never>? {
            defer { value = nil }
            return value.map { .success($0) }
        }

        mutating func cancel() {
            value = nil
        }
    }
    var value: Element
    func makeAsyncIterator() -> AsyncIterator {
        return AsyncIterator(value: value)
    }
}
