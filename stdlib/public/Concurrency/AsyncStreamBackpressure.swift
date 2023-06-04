//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlib 9999, *)
public struct AsyncStreamAlreadyFinishedError: Error {}

@available(SwiftStdlib 5.1, *)
extension AsyncStream {
    /// A mechanism to interface between producer code and an asynchronous stream.
    ///
    /// Use this source to provide elements to the stream by calling one of the `write` methods, then terminate the stream normally
    /// by calling the `finish()` method.
    @available(SwiftStdlib 9999, *)
    public struct Source: Sendable {
        /// A strategy that handles the backpressure of the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public struct BackPressureStrategy: Sendable {
            /// When the high watermark is reached producers will be suspended. All producers will be resumed again once
            /// the low watermark is reached.
            @available(SwiftStdlib 9999, *)
            public static func watermark(low: Int, high: Int) -> BackPressureStrategy {
                BackPressureStrategy(internalBackPressureStrategy: .watermark(.init(low: low, high: high)))
            }

            private init(internalBackPressureStrategy: _InternalBackPressureStrategy) {
                self._internalBackPressureStrategy = internalBackPressureStrategy
            }

            fileprivate let _internalBackPressureStrategy: _InternalBackPressureStrategy
        }

        /// A type that indicates the result of writing elements to the source.
        @available(SwiftStdlib 9999, *)
        @frozen
        public enum WriteResult: Sendable {
            /// A token that is returned when the asynchronous stream's backpressure strategy indicated that production should
            /// be suspended. Use this token to enqueue a callback by  calling the ``enqueueCallback(_:)`` method.
            public struct CallbackToken: Sendable {
                let id: UInt
            }

            /// Indicates that more elements should be produced and written to the source.
            case produceMore

            /// Indicates that a callback should be enqueued.
            ///
            /// The associated token should be passed to the ``enqueueCallback(_:)`` method.
            case enqueueCallback(CallbackToken)
        }

        /// Backing class for the source used to hook a deinit.
        final class _Backing: Sendable {
            let storage: _BackPressuredStorage<Element, Never>

            init(storage: _BackPressuredStorage<Element, Never>) {
                self.storage = storage
            }

            deinit {
                self.storage.sourceDeinitialized()
            }
        }

        /// A callback to invoke when the stream finished.
        ///
        /// The stream finishes and calls this closure in the following cases:
        /// - No iterator was created and the sequence was deinited
        /// - An iterator was created and deinited
        /// - After ``finish(throwing:)`` was called and all elements have been consumed
        /// - The consuming task got cancelled
        @available(SwiftStdlib 9999, *)
        public var onTermination: (@Sendable () -> Void)? {
            set {
                self._backing.storage.onTermination = newValue
            }
            get {
                self._backing.storage.onTermination
            }
        }

        private var _backing: _Backing

        internal init(storage: _BackPressuredStorage<Element, Never>) {
            self._backing = .init(storage: storage)
        }

        /// Writes new elements to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// - Parameter sequence: The elements to write to the asynchronous stream.
        /// - Returns: The result that indicates if more elements should be produced at this time.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) throws -> WriteResult where Element == S.Element, S: Sequence {
            let id = try self._backing.storage.write(contentsOf: sequence)

            if let id {
                return .enqueueCallback(.init(id: id))
            } else {
                return .produceMore
            }
        }

        /// Write the element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// - Parameter element: The element to write to the asynchronous stream.
        /// - Returns: The result that indicates if more elements should be produced at this time.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element) throws -> WriteResult {
            let id = try self._backing.storage.write(contentsOf: CollectionOfOne(element))
        
            if let id {
                return .enqueueCallback(.init(id: id))
            } else {
                return .produceMore
            }
        }

        /// Enqueues a callback that will be invoked once more elements should be produced.
        ///
        /// Call this method after ``write(contentsOf:)`` or ``write(:)`` returned ``WriteResult/enqueueCallback(_:)``.
        ///
        /// - Important: Enqueueing the same token multiple times is not allowed.
        ///
        /// - Parameters:
        ///   - callbackToken: The callback token.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced.
        @available(SwiftStdlib 9999, *)
        public func enqueueCallback(callbackToken: WriteResult.CallbackToken, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            self._backing.storage.enqueueProducer(callbackToken: callbackToken.id, onProduceMore: onProduceMore)
        }

        /// Cancel an enqueued callback.
        ///
        /// Call this method to cancel a callback enqueued by the ``enqueueCallback(callbackToken:onProduceMore:)`` method.
        ///
        /// - Note: This methods supports being called before ``enqueueCallback(callbackToken:onProduceMore:)`` is called and
        /// will mark the passed `callbackToken` as cancelled.
        ///
        /// - Parameter callbackToken: The callback token.
        @available(SwiftStdlib 9999, *)
        public func cancelCallback(callbackToken: WriteResult.CallbackToken) {
            self._backing.storage.cancelProducer(callbackToken: callbackToken.id)
        }

        /// Write new elements to the asynchronous stream and provide a callback which will be invoked once more elements should be produced.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then `onProduceMore` will be invoked with
        /// a `Result.failure`.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced. This callback might be
        ///   invoked during the call to ``write(contentsOf:onProduceMore:)``.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) where Element == S.Element, S: Sequence {
            do {
                let writeResult = try self.write(contentsOf: sequence)

                switch writeResult {
                case .produceMore:
                    onProduceMore(Result<Void, Error>.success(()))

                case .enqueueCallback(let callbackToken):
                    self.enqueueCallback(callbackToken: callbackToken, onProduceMore: onProduceMore)
                }
            } catch {
                onProduceMore(.failure(error))
            }
        }

        /// Writes the element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then `onProduceMore` will be invoked with
        /// a `Result.failure`.
        ///
        /// - Parameters:
        ///   - sequence: The element to write to the asynchronous stream.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced. This callback might be
        ///   invoked during the call to ``write(_:onProduceMore:)``.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            self.write(contentsOf: CollectionOfOne(element), onProduceMore: onProduceMore)
        }

        /// Write new elements to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// This method returns once more elements should be produced.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: Sequence {
            let writeResult = try { try self.write(contentsOf: sequence) }()

            switch writeResult {
            case .produceMore:
                return

            case .enqueueCallback(let callbackToken):
                try await withTaskCancellationHandler {
                    try await withCheckedThrowingContinuation { continuation in
                        self.enqueueCallback(callbackToken: callbackToken, onProduceMore: { result in
                            switch result {
                            case .success():
                                continuation.resume(returning: ())
                            case .failure(let error):
                                continuation.resume(throwing: error)
                            }
                        })
                    }
                } onCancel: {
                    self.cancelCallback(callbackToken: callbackToken)
                }
            }
        }

        /// Write new element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// This method returns once more elements should be produced.
        ///
        /// - Parameters:
        ///   - sequence: The element to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element) async throws {
            try await self.write(contentsOf: CollectionOfOne(element))
        }

        /// Write the elements of the asynchronous sequence to the asynchronous stream.
        ///
        /// This method returns once the provided asynchronous sequence or the  the asynchronous stream finished.
        ///
        /// - Important: This method does not finish the source if consuming the upstream sequence terminated.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: AsyncSequence {
            for try await element in sequence {
                try await self.write(contentsOf: CollectionOfOne(element))
            }
        }

        /// Indicates that the production terminated.
        ///
        /// After all buffered elements are consumed the next iteration point will return `nil`.
        ///
        /// Calling this function more than once has no effect. After calling finish, the stream enters a terminal state and doesn't accept
        /// new elements.
        @available(SwiftStdlib 9999, *)
        public func finish() {
            self._backing.storage.finish(nil)
        }
    }

    /// Initializes a new ``AsyncStream`` and an ``AsyncStream/Source``.
    ///
    /// - Parameters:
    ///   - elementType: The element type of the stream.
    ///   - backPressureStrategy: The backpressure strategy that the stream should use.
    /// - Returns: A tuple containing the stream and its source. The source should be passed to the
    ///   producer while the stream should be passed to the consumer.
    @available(SwiftStdlib 9999, *)
    public static func makeStream(
        of elementType: Element.Type = Element.self,
        backPressureStrategy: Source.BackPressureStrategy
    ) -> (`Self`, Source) {
        let storage = _BackPressuredStorage<Element, Never>(
            backPressureStrategy: backPressureStrategy._internalBackPressureStrategy
        )
        let source = Source(storage: storage)

        return (.init(storage: storage), source)
    }

    init(storage: _BackPressuredStorage<Element, Never>) {
        self._implementation = .backpressured(.init(storage: storage))
    }
}

@available(SwiftStdlib 5.1, *)
extension AsyncThrowingStream {
    /// A mechanism to interface between producer code and an asynchronous stream.
    ///
    /// Use this source to provide elements to the stream by calling one of the `write` methods, then terminate the stream normally
    /// by calling the `finish()` method. You can also use the source's `finish(throwing:)` method to terminate the stream by
    /// throwing an error.
    @available(SwiftStdlib 9999, *)
    public struct Source: Sendable {
        /// A strategy that handles the backpressure of the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public struct BackPressureStrategy: Sendable {
            /// When the high watermark is reached producers will be suspended. All producers will be resumed again once
            /// the low watermark is reached.
            @available(SwiftStdlib 9999, *)
            public static func watermark(low: Int, high: Int) -> BackPressureStrategy {
                BackPressureStrategy(internalBackPressureStrategy: .watermark(.init(low: low, high: high)))
            }

            private init(internalBackPressureStrategy: _InternalBackPressureStrategy) {
                self._internalBackPressureStrategy = internalBackPressureStrategy
            }

            fileprivate let _internalBackPressureStrategy: _InternalBackPressureStrategy
        }

        /// A type that indicates the result of writing elements to the source.
        @available(SwiftStdlib 9999, *)
        @frozen
        public enum WriteResult: Sendable {
            /// A token that is returned when the asynchronous stream's backpressure strategy indicated that production should
            /// be suspended. Use this token to enqueue a callback by  calling the ``enqueueCallback(_:)`` method.
            public struct CallbackToken: Sendable {
                let id: UInt
            }

            /// Indicates that more elements should be produced and written to the source.
            case produceMore

            /// Indicates that a callback should be enqueued.
            ///
            /// The associated token should be passed to the ``enqueueCallback(_:)`` method.
            case enqueueCallback(CallbackToken)
        }

        /// Backing class for the source used to hook a deinit.
        final class _Backing: Sendable {
            let storage: _BackPressuredStorage<Element, Failure>

            init(storage: _BackPressuredStorage<Element, Failure>) {
                self.storage = storage
            }

            deinit {
                self.storage.sourceDeinitialized()
            }
        }

        /// A callback to invoke when the stream finished.
        ///
        /// The stream finishes and calls this closure in the following cases:
        /// - No iterator was created and the sequence was deinited
        /// - An iterator was created and deinited
        /// - After ``finish(throwing:)`` was called and all elements have been consumed
        /// - The consuming task got cancelled
        @available(SwiftStdlib 9999, *)
        public var onTermination: (@Sendable () -> Void)? {
            set {
                self._backing.storage.onTermination = newValue
            }
            get {
                self._backing.storage.onTermination
            }
        }

        private var _backing: _Backing

        internal init(storage: _BackPressuredStorage<Element, Failure>) {
            self._backing = .init(storage: storage)
        }

        /// Writes new elements to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// - Parameter sequence: The elements to write to the asynchronous stream.
        /// - Returns: The result that indicates if more elements should be produced at this time.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) throws -> WriteResult where Element == S.Element, S: Sequence {
            let id = try self._backing.storage.write(contentsOf: sequence)

            if let id {
                return .enqueueCallback(.init(id: id))
            } else {
                return .produceMore
            }
        }

        /// Write the element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// - Parameter element: The element to write to the asynchronous stream.
        /// - Returns: The result that indicates if more elements should be produced at this time.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element) throws -> WriteResult {
            let id = try self._backing.storage.write(contentsOf: CollectionOfOne(element))

            if let id {
                return .enqueueCallback(.init(id: id))
            } else {
                return .produceMore
            }
        }

        /// Enqueues a callback that will be invoked once more elements should be produced.
        ///
        /// Call this method after ``write(contentsOf:)`` or ``write(:)`` returned ``WriteResult/enqueueCallback(_:)``.
        ///
        /// - Important: Enqueueing the same token multiple times is not allowed.
        ///
        /// - Parameters:
        ///   - callbackToken: The callback token.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced.
        @available(SwiftStdlib 9999, *)
        public func enqueueCallback(callbackToken: WriteResult.CallbackToken, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            self._backing.storage.enqueueProducer(callbackToken: callbackToken.id, onProduceMore: onProduceMore)
        }

        /// Cancel an enqueued callback.
        ///
        /// Call this method to cancel a callback enqueued by the ``enqueueCallback(callbackToken:onProduceMore:)`` method.
        ///
        /// - Note: This methods supports being called before ``enqueueCallback(callbackToken:onProduceMore:)`` is called and
        /// will mark the passed `callbackToken` as cancelled.
        ///
        /// - Parameter callbackToken: The callback token.
        @available(SwiftStdlib 9999, *)
        public func cancelCallback(callbackToken: WriteResult.CallbackToken) {
            self._backing.storage.cancelProducer(callbackToken: callbackToken.id)
        }

        /// Write new elements to the asynchronous stream and provide a callback which will be invoked once more elements should be produced.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then `onProduceMore` will be invoked with
        /// a `Result.failure`.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced. This callback might be
        ///   invoked during the call to ``write(contentsOf:onProduceMore:)``.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) where Element == S.Element, S: Sequence {
            do {
                let writeResult = try self.write(contentsOf: sequence)

                switch writeResult {
                case .produceMore:
                    onProduceMore(Result<Void, Error>.success(()))

                case .enqueueCallback(let callbackToken):
                    self.enqueueCallback(callbackToken: callbackToken, onProduceMore: onProduceMore)
                }
            } catch {
                onProduceMore(.failure(error))
            }
        }

        /// Writes the element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then `onProduceMore` will be invoked with
        /// a `Result.failure`.
        ///
        /// - Parameters:
        ///   - sequence: The element to write to the asynchronous stream.
        ///   - onProduceMore: The callback which gets invoked once more elements should be produced. This callback might be
        ///   invoked during the call to ``write(_:onProduceMore:)``.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            self.write(contentsOf: CollectionOfOne(element), onProduceMore: onProduceMore)
        }

        /// Write new elements to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// first element of the provided sequence. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// This method returns once more elements should be produced.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: Sequence {
            let writeResult = try { try self.write(contentsOf: sequence) }()

            switch writeResult {
            case .produceMore:
                return

            case .enqueueCallback(let callbackToken):
                try await withTaskCancellationHandler {
                    try await withCheckedThrowingContinuation { continuation in
                        self.enqueueCallback(callbackToken: callbackToken, onProduceMore: { result in
                            switch result {
                            case .success():
                                continuation.resume(returning: ())
                            case .failure(let error):
                                continuation.resume(throwing: error)
                            }
                        })
                    }
                } onCancel: {
                    self.cancelCallback(callbackToken: callbackToken)
                }
            }
        }

        /// Write new element to the asynchronous stream.
        ///
        /// If there is a task consuming the stream and awaiting the next element then the task will get resumed with the
        /// provided element. If the asynchronous stream already terminated then this method will throw an error
        /// indicating the failure.
        ///
        /// This method returns once more elements should be produced.
        ///
        /// - Parameters:
        ///   - sequence: The element to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write(_ element: Element) async throws {
            try await self.write(contentsOf: CollectionOfOne(element))
        }

        /// Write the elements of the asynchronous sequence to the asynchronous stream.
        ///
        /// This method returns once the provided asynchronous sequence or the  the asynchronous stream finished.
        ///
        /// - Important: This method does not finish the source if consuming the upstream sequence terminated.
        ///
        /// - Parameters:
        ///   - sequence: The elements to write to the asynchronous stream.
        @available(SwiftStdlib 9999, *)
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: AsyncSequence {
            for try await element in sequence {
                try await self.write(contentsOf: CollectionOfOne(element))
            }
        }

        /// Indicates that the production terminated.
        ///
        /// After all buffered elements are consumed the next iteration point will return `nil` or throw an error.
        ///
        /// Calling this function more than once has no effect. After calling finish, the stream enters a terminal state and doesn't accept
        /// new elements.
        ///
        /// - Parameters:
        ///   - error: The error to throw, or `nil`, to finish normally.
        @available(SwiftStdlib 9999, *)
        public func finish(throwing error: Failure?) {
            self._backing.storage.finish(error)
        }
    }

    /// Initializes a new ``AsyncThrowingStream`` and an ``AsyncThrowingStream/Source``.
    ///
    /// - Parameters:
    ///   - elementType: The element type of the stream.
    ///   - failureType: The failure type of the stream.
    ///   - backPressureStrategy: The backpressure strategy that the stream should use.
    /// - Returns: A tuple containing the stream and its source. The source should be passed to the
    ///   producer while the stream should be passed to the consumer.
    @available(SwiftStdlib 9999, *)
    public static func makeStream(
        of elementType: Element.Type = Element.self,
        throwing failureType: Failure.Type = Failure.self,
        backPressureStrategy: Source.BackPressureStrategy
    ) -> (`Self`, Source) where Failure == Error {
        let storage = _BackPressuredStorage<Element, Failure>(
            backPressureStrategy: backPressureStrategy._internalBackPressureStrategy
        )
        let source = Source(storage: storage)

        return (.init(storage: storage), source)
    }

    init(storage: _BackPressuredStorage<Element, Failure>) {
        self._implementation = .backpressured(.init(storage: storage))
    }
}

struct _WatermarkBackPressureStrategy {
    /// The low watermark where demand should start.
    private let _low: Int
    /// The high watermark where demand should be stopped.
    private let _high: Int

    /// Initializes a new ``_WatermarkBackPressureStrategy``.
    ///
    /// - Parameters:
    ///   - low: The low watermark where demand should start.
    ///   - high: The high watermark where demand should be stopped.
    init(low: Int, high: Int) {
        precondition(low <= high)
        self._low = low
        self._high = high
    }

    func didYield(bufferDepth: Int) -> Bool {
        // We are demanding more until we reach the high watermark
        return bufferDepth < self._high
    }

    func didConsume(bufferDepth: Int) -> Bool {
        // We start demanding again once we are below the low watermark
        return bufferDepth < self._low
    }
}

@available(SwiftStdlib 5.1, *)
enum _InternalBackPressureStrategy {
    case watermark(_WatermarkBackPressureStrategy)

    mutating func didYield(bufferDepth: Int) -> Bool {
        switch self {
        case .watermark(let strategy):
            return strategy.didYield(bufferDepth: bufferDepth)
        }
    }

    mutating func didConsume(bufferDepth: Int) -> Bool {
        switch self {
        case .watermark(let strategy):
            return strategy.didConsume(bufferDepth: bufferDepth)
        }
    }
}

// We are unchecked Sendable since we are protecting our state with a lock.
@available(SwiftStdlib 5.1, *)
final class _BackPressuredStorage<Element, Failure: Error>: @unchecked Sendable {
    /// The state machine
    var _stateMachine: _ManagedCriticalState<_StateMachine<Element, Failure>>

    var onTermination: (@Sendable () -> Void)? {
        set {
            self._stateMachine.withCriticalRegion {
                $0._onTermination = newValue
            }
        }
        get {
            self._stateMachine.withCriticalRegion {
                $0._onTermination
            }
        }
    }

    init(
        backPressureStrategy: _InternalBackPressureStrategy
    ) {
        self._stateMachine = .init(.init(backPressureStrategy: backPressureStrategy))
    }

    @available(SwiftStdlib 9999, *)
    func sequenceDeinitialized() {
        let action = self._stateMachine.withCriticalRegion {
            $0.sequenceDeinitialized()
        }

        switch action {
        case .callOnTermination(let onTermination):
            onTermination?()

        case .failProducersAndCallOnTermination(let producerContinuations, let onTermination):
            for producerContinuation in producerContinuations {
                producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
            }
            onTermination?()

        case .none:
            break
        }
    }

    func iteratorInitialized() {
        self._stateMachine.withCriticalRegion {
            $0.iteratorInitialized()
        }
    }

    @available(SwiftStdlib 9999, *)
    func iteratorDeinitialized() {
        let action = self._stateMachine.withCriticalRegion {
            $0.iteratorDeinitialized()
        }

        switch action {
        case .callOnTermination(let onTermination):
            onTermination?()

        case .failProducersAndCallOnTermination(let producerContinuations, let onTermination):
            for producerContinuation in producerContinuations {
                producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
            }
            onTermination?()

        case .none:
            break
        }
    }

    @available(SwiftStdlib 9999, *)
    func sourceDeinitialized() {
        let action = self._stateMachine.withCriticalRegion {
            $0.sourceDeinitialized()
        }

        switch action {
        case .callOnTermination(let onTermination):
            onTermination?()

        case .failProducersAndCallOnTermination(let producerContinuations, let onTermination):
            for producerContinuation in producerContinuations {
                producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
            }
            onTermination?()

        case .failProducers(let producerContinuations):
            for producerContinuation in producerContinuations {
                producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
            }

        case .none:
            break
        }
    }

    @available(SwiftStdlib 9999, *)
    func write(
        contentsOf sequence: some Sequence<Element>
    ) throws -> UInt? {
        let action = self._stateMachine.withCriticalRegion {
            return $0.write(sequence)
        }

        switch action {
        case .returnProduceMore:
            return nil

        case .returnEnqueue(let callbackToken):
            return callbackToken

        case .resumeConsumerAndReturnProduceMore(let continuation, let element):
            continuation.resume(returning: element)
            return nil

        case .resumeConsumerAndReturnEnqueue(let continuation, let element, let callbackToken):
            continuation.resume(returning: element)
            return callbackToken

        case .throwFinishedError:
            throw AsyncStreamAlreadyFinishedError()
        }
    }

    @available(SwiftStdlib 9999, *)
    func enqueueProducer(callbackToken: UInt, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
        let action = self._stateMachine.withCriticalRegion {
            $0.enqueueProducer(callbackToken: callbackToken, onProduceMore: onProduceMore)
        }

        switch action {
        case .resumeProducer(let onProduceMore):
            onProduceMore(Result<Void, Error>.success(()))

        case .resumeProducerWithError(let onProduceMore, let error):
            onProduceMore(Result<Void, Error>.failure(error))

        case .none:
            break
        }
    }

    func cancelProducer(callbackToken: UInt) {
        let action = self._stateMachine.withCriticalRegion {
            $0.cancelProducer(callbackToken: callbackToken)
        }

        switch action {
        case .resumeProducerWithCancellationError(let onProduceMore):
            onProduceMore(Result<Void, Error>.failure(CancellationError()))

        case .none:
            break
        }
    }

    @available(SwiftStdlib 9999, *)
    func finish(_ failure: Failure?) {
        let action = self._stateMachine.withCriticalRegion {
            $0.finish(failure)
        }

        switch action {
        case .callOnTermination(let onTermination):
            onTermination?()

        case .resumeConsumerAndCallOnTermination(let consumerContinuation, let failure, let onTermination):
            switch failure {
            case .some(let error):
                consumerContinuation.resume(throwing: error)
            case .none:
                consumerContinuation.resume(returning: nil)
            }

            onTermination?()

        case .resumeProducers(let producerContinuations):
            for producerContinuation in producerContinuations {
                producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
            }

        case .none:
            break
        }
    }

    @available(SwiftStdlib 9999, *)
    func next() async throws -> Element? {
        let action = self._stateMachine.withCriticalRegion {
            $0.next()
        }

        switch action {
        case .returnElement(let element):
            return element

        case .returnElementAndResumeProducers(let element, let producerContinuations):
            for producerContinuation in producerContinuations {
                producerContinuation(Result<Void, Error>.success(()))
            }

            return element

        case .returnFailureAndCallOnTermination(let failure, let onTermination):
            onTermination?()
            switch failure {
            case .some(let error):
                throw error

            case .none:
                return nil
            }

        case .returnNil:
            return nil

        case .suspendTask:
            return try await self.suspendNext()
        }
    }

    @available(SwiftStdlib 9999, *)
    func suspendNext() async throws -> Element? {
        return try await withTaskCancellationHandler {
            return try await withCheckedThrowingContinuation { continuation in
                let action = self._stateMachine.withCriticalRegion {
                    $0.suspendNext(continuation: continuation)
                }

                switch action {
                case .resumeConsumerWithElement(let continuation, let element):
                    continuation.resume(returning: element)

                case .resumeConsumerWithElementAndProducers(let continuation, let element, let producerContinuations):
                    continuation.resume(returning: element)
                    for producerContinuation in producerContinuations {
                        producerContinuation(Result<Void, Error>.success(()))
                    }

                case .resumeConsumerWithFailureAndCallOnTermination(let continuation, let failure, let onTermination):
                    switch failure {
                    case .some(let error):
                        continuation.resume(throwing: error)

                    case .none:
                        continuation.resume(returning: nil)
                    }
                    onTermination?()

                case .resumeConsumerWithNil(let continuation):
                    continuation.resume(returning: nil)

                case .none:
                    break
                }
            }
        } onCancel: {
            let action = self._stateMachine.withCriticalRegion {
                $0.cancelNext()
            }

            switch action {
            case .resumeConsumerWithCancellationErrorAndCallOnTermination(let continuation, let onTermination):
                continuation.resume(throwing: CancellationError())
                onTermination?()

            case .failProducersAndCallOnTermination(let producerContinuations, let onTermination):
                for producerContinuation in producerContinuations {
                    producerContinuation(.failure(AsyncStreamAlreadyFinishedError()))
                }
                onTermination?()

            case .none:
                break
            }
        }
    }
}

/// The state machine of the backpressured async stream.
@available(SwiftStdlib 5.1, *)
struct _StateMachine<Element, Failure: Error> {
    enum _State {
        struct Initial {
            /// The backpressure strategy.
            var backPressureStrategy: _InternalBackPressureStrategy
            /// Indicates if the iterator was initialized.
            var iteratorInitialized: Bool
            /// The onTermination callback.
            var onTermination: (@Sendable () -> Void)?
        }

        struct Streaming {
            /// The backpressure strategy.
            var backPressureStrategy: _InternalBackPressureStrategy
            /// Indicates if the iterator was initialized.
            var iteratorInitialized: Bool
            /// The onTermination callback.
            var onTermination: (@Sendable () -> Void)?
            /// The buffer of elements.
            var buffer: _Deque<Element>
            /// The optional consumer continuation.
            var consumerContinuation: CheckedContinuation<Element?, Error>?
            /// The producer continuations.
            var producerContinuations: _Deque<(UInt, (Result<Void, Error>) -> Void)>
            /// The producers that have been cancelled.
            var cancelledAsyncProducers: _Deque<UInt>
            /// Indicates if we currently have outstanding demand.
            var hasOutstandingDemand: Bool
        }

        struct SourceFinished {
            /// Indicates if the iterator was initialized.
            var iteratorInitialized: Bool
            /// The buffer of elements.
            var buffer: _Deque<Element>
            /// The failure that should be thrown after the last element has been consumed.
            var failure: Failure?
            /// The onTermination callback.
            var onTermination: (@Sendable () -> Void)?
        }

        case initial(Initial)
        /// The state once either any element was yielded or `next()` was called.
        case streaming(Streaming)
        /// The state once the underlying source signalled that it is finished.
        case sourceFinished(SourceFinished)

        /// The state once there can be no outstanding demand. This can happen if:
        /// 1. The iterator was deinited
        /// 2. The underlying source finished and all buffered elements have been consumed
        case finished(iteratorInitialized: Bool)

        /// An intermediate state to avoid CoWs.
        case modify
    }

    /// The state machine's current state.
    var _state: _State

    // The ID used for the next CallbackToken.
    var nextCallbackTokenID: UInt = 0

    var _onTermination: (@Sendable () -> Void)? {
        set {
            switch self._state {
            case .initial(var initial):
                initial.onTermination = newValue
                self._state = .initial(initial)

            case .streaming(var streaming):
                streaming.onTermination = newValue
                self._state = .streaming(streaming)

            case .sourceFinished(var sourceFinished):
                sourceFinished.onTermination = newValue
                self._state = .sourceFinished(sourceFinished)

            case .finished:
                break

            case .modify:
                fatalError("AsyncStream internal inconsistency")
            }
        }
        get {
            switch self._state {
            case .initial(let initial):
                return initial.onTermination

            case .streaming(let streaming):
                return streaming.onTermination

            case .sourceFinished(let sourceFinished):
                return sourceFinished.onTermination

            case .finished:
                return nil

            case .modify:
                fatalError("AsyncStream internal inconsistency")
            }
        }
    }

    /// Initializes a new `StateMachine`.
    ///
    /// We are passing and holding the back-pressure strategy here because
    /// it is a customizable extension of the state machine.
    ///
    /// - Parameter backPressureStrategy: The back-pressure strategy.
    init(
        backPressureStrategy: _InternalBackPressureStrategy
    ) {
        self._state = .initial(.init(
            backPressureStrategy: backPressureStrategy,
            iteratorInitialized: false,
            onTermination: nil
        ))
    }

    /// Generates the next callback token.
    mutating func nextCallbackToken() -> UInt {
        let id = self.nextCallbackTokenID
        self.nextCallbackTokenID += 1
        return id
    }

    /// Actions returned by `sequenceDeinitialized()`.
    enum SequenceDeinitializedAction {
        /// Indicates that `onTermination` should be called.
        case callOnTermination((@Sendable () -> Void)?)
        /// Indicates that  all producers should be failed and `onTermination` should be called.
        case failProducersAndCallOnTermination(
            [(Result<Void, Error>) -> Void],
            (@Sendable () -> Void)?
        )
    }

    mutating func sequenceDeinitialized() -> SequenceDeinitializedAction? {
        switch self._state {
        case .initial(let initial):
            if initial.iteratorInitialized {
                // An iterator was created and we deinited the sequence.
                // This is an expected pattern and we just continue on normal.
                return .none
            } else {
                // No iterator was created so we can transition to finished right away.
                self._state = .finished(iteratorInitialized: false)

                return .callOnTermination(initial.onTermination)
            }

        case .streaming(let streaming):
            if streaming.iteratorInitialized {
                // An iterator was created and we deinited the sequence.
                // This is an expected pattern and we just continue on normal.
                return .none
            } else {
                // No iterator was created so we can transition to finished right away.
                self._state = .finished(iteratorInitialized: false)

                return .failProducersAndCallOnTermination(
                    Array(streaming.producerContinuations.map { $0.1 }),
                    streaming.onTermination
                )
            }

        case .sourceFinished(let sourceFinished):
            if sourceFinished.iteratorInitialized {
                // An iterator was created and we deinited the sequence.
                // This is an expected pattern and we just continue on normal.
                return .none
            } else {
                // No iterator was created so we can transition to finished right away.
                self._state = .finished(iteratorInitialized: false)

                return .callOnTermination(sourceFinished.onTermination)
            }

        case .finished:
            // We are already finished so there is nothing left to clean up.
            // This is just the references dropping afterwards.
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    mutating func iteratorInitialized() {
        switch self._state {
        case .initial(var initial):
            if initial.iteratorInitialized {
                // Our sequence is a unicast sequence and does not support multiple AsyncIterator's
                fatalError("Only a single AsyncIterator can be created")
            } else {
                // The first and only iterator was initialized.
                initial.iteratorInitialized = true
                self._state = .initial(initial)
            }

        case .streaming(var streaming):
            if streaming.iteratorInitialized {
                // Our sequence is a unicast sequence and does not support multiple AsyncIterator's
                fatalError("Only a single AsyncIterator can be created")
            } else {
                // The first and only iterator was initialized.
                streaming.iteratorInitialized = true
                self._state = .streaming(streaming)
            }

        case .sourceFinished(var sourceFinished):
            if sourceFinished.iteratorInitialized {
                // Our sequence is a unicast sequence and does not support multiple AsyncIterator's
                fatalError("Only a single AsyncIterator can be created")
            } else {
                // The first and only iterator was initialized.
                sourceFinished.iteratorInitialized = true
                self._state = .sourceFinished(sourceFinished)
            }

        case .finished(iteratorInitialized: true):
            // Our sequence is a unicast sequence and does not support multiple AsyncIterator's
            fatalError("Only a single AsyncIterator can be created")

        case .finished(iteratorInitialized: false):
            // It is strange that an iterator is created after we are finished
            // but it can definitely happen, e.g.
            // Sequence.init -> source.finish -> sequence.makeAsyncIterator
            self._state = .finished(iteratorInitialized: true)

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `iteratorDeinitialized()`.
    enum IteratorDeinitializedAction {
        /// Indicates that `onTermination` should be called.
        case callOnTermination((@Sendable () -> Void)?)
        /// Indicates that  all producers should be failed and `onTermination` should be called.
        case failProducersAndCallOnTermination(
            [(Result<Void, Error>) -> Void],
            (@Sendable () -> Void)?
        )
    }

    mutating func iteratorDeinitialized() -> IteratorDeinitializedAction? {
        switch self._state {
        case .initial(let initial):
            if initial.iteratorInitialized {
                // An iterator was created and deinited. Since we only support
                // a single iterator we can now transition to finish.
                self._state = .finished(iteratorInitialized: true)
                return .callOnTermination(initial.onTermination)
            } else {
                // An iterator needs to be initialized before it can be deinitialized.
                fatalError("AsyncStream internal inconsistency")
            }

        case .streaming(let streaming):
            if streaming.iteratorInitialized {
                // An iterator was created and deinited. Since we only support
                // a single iterator we can now transition to finish.
                self._state = .finished(iteratorInitialized: true)

                return .failProducersAndCallOnTermination(
                    Array(streaming.producerContinuations.map { $0.1 }),
                    streaming.onTermination
                )
            } else {
                // An iterator needs to be initialized before it can be deinitialized.
                fatalError("AsyncStream internal inconsistency")
            }

        case .sourceFinished(let sourceFinished):
            if sourceFinished.iteratorInitialized {
                // An iterator was created and deinited. Since we only support
                // a single iterator we can now transition to finish.
                self._state = .finished(iteratorInitialized: true)
                return .callOnTermination(sourceFinished.onTermination)
            } else {
                // An iterator needs to be initialized before it can be deinitialized.
                fatalError("AsyncStream internal inconsistency")
            }

        case .finished:
            // We are already finished so there is nothing left to clean up.
            // This is just the references dropping afterwards.
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `sourceDeinitialized()`.
    enum SourceDeinitializedAction {
        /// Indicates that `onTermination` should be called.
        case callOnTermination((() -> Void)?)
        /// Indicates that  all producers should be failed and `onTermination` should be called.
        case failProducersAndCallOnTermination(
            [(Result<Void, Error>) -> Void],
            (@Sendable () -> Void)?
        )
        /// Indicates that all producers should be failed.
        case failProducers([(Result<Void, Error>) -> Void])
    }

    mutating func sourceDeinitialized() -> SourceDeinitializedAction? {
        switch self._state {
        case .initial(let initial):
            // The source got deinited before anything was written
            self._state = .finished(iteratorInitialized: initial.iteratorInitialized)
            return .callOnTermination(initial.onTermination)

        case .streaming(let streaming):
            if streaming.buffer.isEmpty {
                // We can transition to finished right away since the buffer is empty now
                self._state = .finished(iteratorInitialized: streaming.iteratorInitialized)

                return .failProducersAndCallOnTermination(
                    Array(streaming.producerContinuations.map { $0.1 }),
                    streaming.onTermination
                )
            } else {
                // The continuation must be `nil` if the buffer has elements
                precondition(streaming.consumerContinuation == nil)

                self._state = .sourceFinished(.init(
                    iteratorInitialized: streaming.iteratorInitialized,
                    buffer: streaming.buffer,
                    failure: nil,
                    onTermination: streaming.onTermination
                ))

                return .failProducers(
                    Array(streaming.producerContinuations.map { $0.1 })
                )
            }

        case .sourceFinished, .finished:
            // This is normal and we just have to tolerate it
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `write()`.
    enum WriteAction {
        /// Indicates that the producer should be notified to produce more.
        case returnProduceMore
        /// Indicates that the producer should be suspended to stop producing.
        case returnEnqueue(
            callbackToken: UInt
        )
        /// Indicates that the consumer should be resumed and the producer should be notified to produce more.
        case resumeConsumerAndReturnProduceMore(
            continuation: CheckedContinuation<Element?, Error>,
            element: Element
        )
        /// Indicates that the consumer should be resumed and the producer should be suspended.
        case resumeConsumerAndReturnEnqueue(
            continuation: CheckedContinuation<Element?, Error>,
            element: Element,
            callbackToken: UInt
        )
        /// Indicates that the producer has been finished.
        case throwFinishedError

        init(
            callbackToken: UInt?,
            continuationAndElement: (CheckedContinuation<Element?, Error>, Element)? = nil
        ) {
            switch (callbackToken, continuationAndElement) {
            case (.none, .none):
                self = .returnProduceMore

            case (.some(let callbackToken), .none):
                self = .returnEnqueue(callbackToken: callbackToken)

            case (.none, .some((let continuation, let element))):
                self = .resumeConsumerAndReturnProduceMore(
                    continuation: continuation,
                    element: element
                )

            case (.some(let callbackToken), .some((let continuation, let element))):
                self = .resumeConsumerAndReturnEnqueue(
                    continuation: continuation,
                    element: element,
                    callbackToken: callbackToken
                )
            }
        }
    }

    mutating func write(_ sequence: some Sequence<Element>) -> WriteAction {
        switch self._state {
        case .initial(var initial):
            var buffer = _Deque<Element>()
            buffer.append(contentsOf: sequence)

            let shouldProduceMore = initial.backPressureStrategy.didYield(bufferDepth: buffer.count)
            let callbackToken = shouldProduceMore ? nil : self.nextCallbackToken()

            self._state = .streaming(.init(
                backPressureStrategy: initial.backPressureStrategy,
                iteratorInitialized: initial.iteratorInitialized,
                onTermination: initial.onTermination,
                buffer: buffer,
                consumerContinuation: nil,
                producerContinuations: .init(),
                cancelledAsyncProducers: .init(),
                hasOutstandingDemand: shouldProduceMore
            ))

            return .init(callbackToken: callbackToken)

        case .streaming(var streaming):
            self._state = .modify

            streaming.buffer.append(contentsOf: sequence)

            // We have an element and can resume the continuation
            let shouldProduceMore = streaming.backPressureStrategy.didYield(bufferDepth: streaming.buffer.count)
            streaming.hasOutstandingDemand = shouldProduceMore
            let callbackToken = shouldProduceMore ? nil : self.nextCallbackToken()

            if let consumerContinuation = streaming.consumerContinuation {
                guard let element = streaming.buffer.popFirst() else {
                    // We got a yield of an empty sequence. We just tolerate this.
                    self._state = .streaming(streaming)

                    return .init(callbackToken: callbackToken)
                }

                // We got a consumer continuation and an element. We can resume the consumer now
                streaming.consumerContinuation = nil
                self._state = .streaming(streaming)
                return .init(
                    callbackToken: callbackToken,
                    continuationAndElement: (consumerContinuation, element)
                )
            } else {
                // We don't have a suspended consumer so we just buffer the elements
                self._state = .streaming(streaming)
                return .init(
                    callbackToken: callbackToken
                )
            }

        case .sourceFinished, .finished:
            // If the source has finished we are dropping the elements.
            return .throwFinishedError

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `enqueueProducer()`.
    enum EnqueueProducerAction {
        /// Indicates that the producer should be notified to produce more.
        case resumeProducer((Result<Void, Error>) -> Void)
        /// Indicates that the producer should be notified about an error.
        case resumeProducerWithError((Result<Void, Error>) -> Void, Error)
    }

    @available(SwiftStdlib 9999, *)
    mutating func enqueueProducer(callbackToken: UInt, onProduceMore: @Sendable @escaping (Result<Void, Error>) -> Void) -> EnqueueProducerAction? {
        switch self._state {
        case .initial:
            // We need to transition to streaming before we can suspend
            // This is enforced because the CallbackToken has no public init so
            // one must create it by calling `write` first.
            fatalError("AsyncStream internal inconsistency")

        case .streaming(var streaming):
            if let index = streaming.cancelledAsyncProducers.firstIndex(of: callbackToken) {
                // Our producer got marked as cancelled.
                self._state = .modify
                streaming.cancelledAsyncProducers.remove(at: index)
                self._state = .streaming(streaming)

                return .resumeProducerWithError(onProduceMore, CancellationError())
            } else if streaming.hasOutstandingDemand {
                // We hit an edge case here where we wrote but the consuming thread got interleaved
                return .resumeProducer(onProduceMore)
            } else {
                self._state = .modify
                streaming.producerContinuations.append((callbackToken, onProduceMore))

                self._state = .streaming(streaming)
                return .none
            }

        case .sourceFinished, .finished:
            // Since we are unlocking between yielding and suspending the yield
            // It can happen that the source got finished or the consumption fully finishes.
            return .resumeProducerWithError(onProduceMore, AsyncStreamAlreadyFinishedError())

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `cancelProducer()`.
    enum CancelProducerAction {
        /// Indicates that the producer should be notified about cancellation.
        case resumeProducerWithCancellationError((Result<Void, Error>) -> Void)
    }

    mutating func cancelProducer(callbackToken: UInt) -> CancelProducerAction? {
        switch self._state {
        case .initial:
            // We need to transition to streaming before we can suspend
            fatalError("AsyncStream internal inconsistency")

        case .streaming(var streaming):
            if let index = streaming.producerContinuations.firstIndex(where: { $0.0 == callbackToken }) {
                // We have an enqueued producer that we need to resume now
                self._state = .modify
                let continuation = streaming.producerContinuations.remove(at: index).1
                self._state = .streaming(streaming)

                return .resumeProducerWithCancellationError(continuation)
            } else {
                // The task that yields was cancelled before yielding so the cancellation handler
                // got invoked right away
                self._state = .modify
                streaming.cancelledAsyncProducers.append(callbackToken)
                self._state = .streaming(streaming)

                return .none
            }

        case .sourceFinished, .finished:
            // Since we are unlocking between yielding and suspending the yield
            // It can happen that the source got finished or the consumption fully finishes.
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `finish()`.
    enum FinishAction {
        /// Indicates that `onTermination` should be called.
        case callOnTermination((() -> Void)?)
        /// Indicates that the consumer  should be resumed with the failure, the producers
        /// should be resumed with an error and `onTermination` should be called.
        case resumeConsumerAndCallOnTermination(
            consumerContinuation: CheckedContinuation<Element?, Error>,
            failure: Failure?,
            onTermination: (() -> Void)?
        )
        /// Indicates that the producers should be resumed with an error.
        case resumeProducers(
            producerContinuations: Array<(Result<Void, Error>) -> Void>
        )
    }

    @inlinable
    mutating func finish(_ failure: Failure?) -> FinishAction? {
        switch self._state {
        case .initial(let initial):
            // Nothing was yielded nor did anybody call next
            // This means we can transition to sourceFinished and store the failure
            self._state = .sourceFinished(.init(
                iteratorInitialized: initial.iteratorInitialized,
                buffer: .init(),
                failure: failure,
                onTermination: initial.onTermination
            ))

            return .callOnTermination(initial.onTermination)

        case .streaming(let streaming):
            if let consumerContinuation = streaming.consumerContinuation {
                // We have a continuation, this means our buffer must be empty
                // Furthermore, we can now transition to finished
                // and resume the continuation with the failure
                precondition(streaming.buffer.isEmpty, "Expected an empty buffer")
                precondition(streaming.producerContinuations.isEmpty, "Expected no suspended producers")

                self._state = .finished(iteratorInitialized: streaming.iteratorInitialized)

                return .resumeConsumerAndCallOnTermination(
                    consumerContinuation: consumerContinuation,
                    failure: failure,
                    onTermination: streaming.onTermination
                )
            } else {
                self._state = .sourceFinished(.init(
                    iteratorInitialized: streaming.iteratorInitialized,
                    buffer: streaming.buffer,
                    failure: failure,
                    onTermination: streaming.onTermination
                ))

                return .resumeProducers(producerContinuations: Array(streaming.producerContinuations.map { $0.1 }))
            }

        case .sourceFinished, .finished:
            // If the source has finished, finishing again has no effect.
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `next()`.
    enum NextAction {
        /// Indicates that the element should be returned to the caller.
        case returnElement(Element)
        /// Indicates that the element should be returned to the caller and that all producers should be called.
        case returnElementAndResumeProducers(Element, [(Result<Void, Error>) -> Void])
        /// Indicates that the `Failure` should be returned to the caller and that `onTermination` should be called.
        case returnFailureAndCallOnTermination(Failure?, (() -> Void)?)
        /// Indicates that the `nil` should be returned to the caller.
        case returnNil
        /// Indicates that the `Task` of the caller should be suspended.
        case suspendTask
    }

    mutating func next() -> NextAction {
        switch self._state {
        case .initial(let initial):
            // We are not interacting with the back-pressure strategy here because
            // we are doing this inside `next(:)`
            self._state = .streaming(.init(
                backPressureStrategy: initial.backPressureStrategy,
                iteratorInitialized: initial.iteratorInitialized,
                onTermination: initial.onTermination,
                buffer: _Deque<Element>(),
                consumerContinuation: nil,
                producerContinuations: .init(),
                cancelledAsyncProducers: .init(),
                hasOutstandingDemand: false
            ))

            return .suspendTask
        case .streaming(var streaming):
            guard streaming.consumerContinuation == nil else {
                // We have multiple AsyncIterators iterating the sequence
                fatalError("AsyncStream internal inconsistency")
            }

            self._state = .modify

            if let element = streaming.buffer.popFirst() {
                // We have an element to fulfil the demand right away.
                let shouldProduceMore = streaming.backPressureStrategy.didConsume(bufferDepth: streaming.buffer.count)
                streaming.hasOutstandingDemand = shouldProduceMore

                if shouldProduceMore {
                    // There is demand and we have to resume our producers
                    let producers = Array(streaming.producerContinuations.map { $0.1 })
                    streaming.producerContinuations.removeAll()
                    self._state = .streaming(streaming)
                    return .returnElementAndResumeProducers(element, producers)
                } else {
                    // We don't have any new demand, so we can just return the element.
                    self._state = .streaming(streaming)
                    return .returnElement(element)
                }
            } else {
                // There is nothing in the buffer to fulfil the demand so we need to suspend.
                // We are not interacting with the back-pressure strategy here because
                // we are doing this inside `suspendNext`
                self._state = .streaming(streaming)

                return .suspendTask
            }

        case .sourceFinished(var sourceFinished):
            // Check if we have an element left in the buffer and return it
            self._state = .modify

            if let element = sourceFinished.buffer.popFirst() {
                self._state = .sourceFinished(sourceFinished)

                return .returnElement(element)
            } else {
                // We are returning the queued failure now and can transition to finished
                self._state = .finished(iteratorInitialized: sourceFinished.iteratorInitialized)

                return .returnFailureAndCallOnTermination(sourceFinished.failure, sourceFinished.onTermination)
            }

        case .finished:
            return .returnNil

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `suspendNext()`.
    enum SuspendNextAction {
        /// Indicates that the consumer should be resumed.
        case resumeConsumerWithElement(CheckedContinuation<Element?, Error>, Element)
        /// Indicates that the consumer and all producers should be resumed.
        case resumeConsumerWithElementAndProducers(CheckedContinuation<Element?, Error>, Element, [(Result<Void, Error>) -> Void])
        /// Indicates that the consumer should be resumed with the failure and that `onTermination` should be called.
        case resumeConsumerWithFailureAndCallOnTermination(CheckedContinuation<Element?, Error>, Failure?, (() -> Void)?)
        /// Indicates that the consumer should be resumed with `nil`.
        case resumeConsumerWithNil(CheckedContinuation<Element?, Error>)
    }

    mutating func suspendNext(continuation: CheckedContinuation<Element?, Error>) -> SuspendNextAction? {
        switch self._state {
        case .initial:
            // We need to transition to streaming before we can suspend
            preconditionFailure("AsyncStream internal inconsistency")

        case .streaming(var streaming):
            guard streaming.consumerContinuation == nil else {
                // We have multiple AsyncIterators iterating the sequence
                fatalError("This should never happen since we only allow a single Iterator to be created")
            }

            self._state = .modify

            // We have to check here again since we might have a producer interleave next and suspendNext
            if let element = streaming.buffer.popFirst() {
                // We have an element to fulfil the demand right away.

                let shouldProduceMore = streaming.backPressureStrategy.didConsume(bufferDepth: streaming.buffer.count)
                streaming.hasOutstandingDemand = shouldProduceMore

                if shouldProduceMore {
                    // There is demand and we have to resume our producers
                    let producers = Array(streaming.producerContinuations.map { $0.1 })
                    streaming.producerContinuations.removeAll()
                    self._state = .streaming(streaming)
                    return .resumeConsumerWithElementAndProducers(continuation, element, producers)
                } else {
                    // We don't have any new demand, so we can just return the element.
                    self._state = .streaming(streaming)
                    return .resumeConsumerWithElement(continuation, element)
                }
            } else {
                // There is nothing in the buffer to fulfil the demand so we to store the continuation.
                streaming.consumerContinuation = continuation
                self._state = .streaming(streaming)

                return .none
            }

        case .sourceFinished(var sourceFinished):
            // Check if we have an element left in the buffer and return it
            self._state = .modify

            if let element = sourceFinished.buffer.popFirst() {
                self._state = .sourceFinished(sourceFinished)

                return .resumeConsumerWithElement(continuation, element)
            } else {
                // We are returning the queued failure now and can transition to finished
                self._state = .finished(iteratorInitialized: sourceFinished.iteratorInitialized)

                return .resumeConsumerWithFailureAndCallOnTermination(
                    continuation,
                    sourceFinished.failure,
                    sourceFinished.onTermination
                )
            }

        case .finished:
            return .resumeConsumerWithNil(continuation)

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }

    /// Actions returned by `cancelNext()`.
    enum CancelNextAction {
        /// Indicates that the continuation should be resumed with a cancellation error, the producers should be finished and call onTermination.
        case resumeConsumerWithCancellationErrorAndCallOnTermination(CheckedContinuation<Element?, Error>, (() -> Void)?)
        /// Indicates that the producers should be finished and call onTermination.
        case failProducersAndCallOnTermination([(Result<Void, Error>) -> Void], (() -> Void)?)
    }

    mutating func cancelNext() -> CancelNextAction? {
        switch self._state {
        case .initial:
            // We need to transition to streaming before we can suspend
            fatalError("AsyncStream internal inconsistency")

        case .streaming(let streaming):
            self._state = .finished(iteratorInitialized: streaming.iteratorInitialized)

            if let consumerContinuation = streaming.consumerContinuation {
                precondition(streaming.producerContinuations.isEmpty, "Internal inconsistency. Unexpected producer continuations.")
                return .resumeConsumerWithCancellationErrorAndCallOnTermination(
                    consumerContinuation,
                    streaming.onTermination
                )
            } else {
                return .failProducersAndCallOnTermination(
                    Array(streaming.producerContinuations.map { $0.1 }),
                    streaming.onTermination
                )
            }

        case .sourceFinished, .finished:
            return .none

        case .modify:
            fatalError("AsyncStream internal inconsistency")
        }
    }
}
#else
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
extension AsyncStream {
    @available(SwiftStdlib 9999, *)
    @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
    public struct Source: Sendable {
        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public struct BackPressureStrategy: Sendable {
            @available(SwiftStdlib 9999, *)
            @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
            public static func watermark(low: Int, high: Int) -> BackPressureStrategy {
                fatalError("Unavailable in task-to-thread concurrency model")
            }
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public enum WriteResult: Sendable {
            @available(SwiftStdlib 9999, *)
            @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
            public struct CallbackToken: Sendable {}
            case produceMore
            case enqueueCallback(CallbackToken)
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public var onTermination: (@Sendable () -> Void)? {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) throws -> WriteResult where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element) throws -> WriteResult {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func enqueueCallback(callbackToken: WriteResult.CallbackToken, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func cancelCallback(callbackToken: WriteResult.CallbackToken) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element) async throws {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: AsyncSequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func finish() {
            fatalError("Unavailable in task-to-thread concurrency model")
        }
    }

    @available(SwiftStdlib 9999, *)
    @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
    public static func makeStream(
        of elementType: Element.Type = Element.self,
        backPressureStrategy: Source.BackPressureStrategy
    ) -> (`Self`, Source) {
            fatalError("Unavailable in task-to-thread concurrency model")
    }
}
@available(SwiftStdlib 5.1, *)
@available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
extension AsyncThrowingStream {
    @available(SwiftStdlib 9999, *)
    @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
    public struct Source: Sendable {
        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public struct BackPressureStrategy: Sendable {
            @available(SwiftStdlib 9999, *)
            @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
            public static func watermark(low: Int, high: Int) -> BackPressureStrategy {
                fatalError("Unavailable in task-to-thread concurrency model")
            }
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public enum WriteResult: Sendable {
            @available(SwiftStdlib 9999, *)
            @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
            public struct CallbackToken: Sendable {}
            case produceMore
            case enqueueCallback(CallbackToken)
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public var onTermination: (@Sendable () -> Void)? {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) throws -> WriteResult where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element) throws -> WriteResult {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func enqueueCallback(callbackToken: WriteResult.CallbackToken, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func cancelCallback(callbackToken: WriteResult.CallbackToken) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element, onProduceMore: @escaping @Sendable (Result<Void, Error>) -> Void) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: Sequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write(_ element: Element) async throws {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func write<S>(contentsOf sequence: S) async throws where Element == S.Element, S: AsyncSequence {
            fatalError("Unavailable in task-to-thread concurrency model")
        }

        @available(SwiftStdlib 9999, *)
        @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
        public func finish(throwing error: Failure?) {
            fatalError("Unavailable in task-to-thread concurrency model")
        }
    }

    @available(SwiftStdlib 9999, *)
    @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
    public static func makeStream(
        of elementType: Element.Type = Element.self,
        backPressureStrategy: Source.BackPressureStrategy
    ) -> (`Self`, Source) {
            fatalError("Unavailable in task-to-thread concurrency model")
    }
}
#endif
