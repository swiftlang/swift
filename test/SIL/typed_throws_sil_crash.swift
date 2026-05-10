// RUN: %target-swift-frontend -sil-verify-all -module-name=test -emit-sil %s -enable-builtin-module

import Builtin

// Ensure compiler does not crash
struct Wrapper {
  var a = [1, 2, 3]
  init() {
    _withUnsafeMutablePointer(to: &a) {
      bar($0)
    }
  }
}

func _withUnsafeMutablePointer<T, E: Error, Result>(
  to value: inout T,
  _ body: (UnsafeMutablePointer<T>) throws(E) -> Result
) throws(E) -> Result {
  try body(UnsafeMutablePointer<T>(Builtin.addressof(&value)))
}

func bar<T>(_ arg: UnsafeMutablePointer<[T]>) {}


/////
// existential erasure crasher (rdar://130981481)
func f(_ x: @escaping ((any Error)?) -> Void) {
    let a = A()
    g(x) { [a] in
        try a.h()
    }
}
func g<F: Error>(
    _ x: @escaping (F) -> (),
    _ y: @escaping () throws(F) -> ()
) { }
struct A {
    func h() throws { }
}

//////
// no-throw <-> Never reabstraction thunk crasher (rdar://142387547)
final class TaskContainer<Success: Sendable, Failure: Error> {
    typealias Task = _Concurrency.Task<Success, Failure>

    struct Identifier {
        init(_ container: TaskContainer) {}
    }

    init<OperationError: Error, E: Error>(
        wrapping operation: sending @escaping @isolated(any) (
            Identifier
        ) async throws(OperationError) -> Success,
        builder: (
            _ container: Identifier,
            sending @escaping @isolated(any) () async throws(OperationError) -> Success
        ) throws(E) -> Task
    ) throws(E) {
        let identifier = Identifier(self)
        _ = try builder(identifier, { () async throws(OperationError) -> Success in return try await operation(identifier) })
    }
}

final class FixedContainer<Success: Sendable, Failure: Error> {
    typealias Task = _Concurrency.Task<Success, Failure>

    struct Identifier {
        init(_ container: FixedContainer) {}
    }

    init<E: Error>(
        wrapping operation: sending @escaping @isolated(any) (
            Identifier
        ) async throws(Never) -> Success,
        builder: (
            _ container: Identifier,
            sending @escaping @isolated(any) () async throws(Never) -> Success
        ) throws(E) -> Task
    ) throws(E) where Failure == Never {
        let identifier = Identifier(self)
        _ = try builder(identifier, { () async throws(Never) -> Success in return await operation(identifier) })
    }

    init<OperationError: Error, E: Error>(
        wrapping operation: sending @escaping @isolated(any) (
            Identifier
        ) async throws(OperationError) -> Success,
        builder: (
            _ container: Identifier,
            sending @escaping @isolated(any) () async throws(OperationError) -> Success
        ) throws(E) -> Task
    ) throws(E) where Failure == any Error {
        let identifier = Identifier(self)
        _ = try builder(identifier, { () async throws(OperationError) -> Success in return try await operation(identifier) })
    }
}

func a<Success: Sendable>(
    operation: sending @escaping @isolated(any) (
        TaskContainer<Success, Never>.Identifier
    ) async -> Success
) -> TaskContainer<Success, Never> {
    typealias Return = TaskContainer<Success, Never>
    return Return(wrapping : operation, builder: { (_, wrappedOperation) -> Return.Task in return Return.Task(operation: wrappedOperation) }) // Return.Task == Task<Success, Never>
}

func a<Success: Sendable, Failure: Error>(
    operation: sending @escaping @isolated(any) (
        TaskContainer<Success, any Error>.Identifier
    ) async throws(Failure) -> Success
) -> TaskContainer<Success, any Error> {
    typealias Return = TaskContainer<Success, any Error>
    return Return(wrapping : operation, builder: { (_, wrappedOperation) -> Return.Task in return Return.Task(operation: wrappedOperation) }) // Return.Task == Task<Success, any Error>
}

func b<Success: Sendable>(
    operation: sending @escaping @isolated(any) (
        FixedContainer<Success, Never>.Identifier
    ) async -> Success
) -> FixedContainer<Success, Never> {
    typealias Return = FixedContainer<Success, Never>
    return Return(wrapping : operation, builder: { (_, wrappedOperation) -> Return.Task in return Return.Task(operation: wrappedOperation) })
}

func b<Success: Sendable, Failure: Error>(
    operation: sending @escaping @isolated(any) (
        FixedContainer<Success, any Error>.Identifier
    ) async throws(Failure) -> Success
) -> FixedContainer<Success, any Error> {
    typealias Return = FixedContainer<Success, any Error>
    return Return(wrapping : operation, builder: { (_, wrappedOperation) -> Return.Task in return Return.Task(operation: wrappedOperation) })
}
