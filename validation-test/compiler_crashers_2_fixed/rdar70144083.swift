// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-concurrency

// REQUIRES: concurrency

public protocol AsyncIteratorProtocol {
    associatedtype Element
    associatedtype Failure: Error

    mutating func nextResult() async -> Result<Element, Failure>?
    mutating func cancel()
}

public protocol AsyncSequence {
    associatedtype Element
    associatedtype Failure: Error
    associatedtype AsyncIterator: AsyncIteratorProtocol where AsyncIterator.Element == Element, AsyncIterator.Failure == Failure
    
    func makeAsyncIterator() -> AsyncIterator
}

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
