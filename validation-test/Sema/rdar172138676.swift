// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: objc_interop

import Combine
import Dispatch

// This bug is easier to trigger with the pre-typed throws version of Task.init,
// so cook that up here.
struct Task<Success: Sendable, Failure: Error> {
    public init(name: String? = nil,
                priority: TaskPriority? = nil,
                operation: sending @escaping @isolated(any) () async -> Success)
        where Failure == Never {}

    public init(name: String? = nil,
                priority: TaskPriority? = nil,
                operation: sending @escaping @isolated(any) () async throws -> Success)
        where Failure == any Error {}
}

extension Sequence {
    func asyncFlatMap<T>(_ transform: (Element) async throws -> [T]) async rethrows -> [T] {
        fatalError()
    }
}

extension Future where Failure == Error {
    convenience init(_: @escaping () async throws -> Output) {
        fatalError()
    }
}

func setUpObservations(subject: PassthroughSubject<Int, Error>) -> AnyCancellable? {
    return subject.subscribe(on: DispatchQueue.main)
        .flatMap { (_: Int) -> AnyPublisher<[Any]?, Error> in
            Future { return [Any]() }.eraseToAnyPublisher()
        }
        .compactMap { $0 }
        .sink(receiveCompletion: { _ in }, receiveValue: { _ in Task { } })
        // expected-warning@-1 {{result of 'Task<Success, Failure>' initializer is unused}}
}

