//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Only support 64bit
#if !(os(iOS) && (arch(i386) || arch(arm)))

@_exported import Foundation // Clang module
import Combine

// The following protocol is so that we can reference `Self` in the Publisher
// below. This is based on a trick used in the the standard library's
// implementation of `NSObject.observe(key path)`
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol _KeyValueCodingAndObservingPublishing {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension NSObject: _KeyValueCodingAndObservingPublishing {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension _KeyValueCodingAndObservingPublishing where Self: NSObject {
    /// Publish values when the value identified by a KVO-compliant keypath changes.
    ///
    /// - Parameters:
    ///   - keyPath: The keypath of the property to publish.
    ///   - options: Key-value observing options.
    /// - Returns: A publisher that emits elements each time the property’s value changes.
    public func publisher<Value>(for keyPath: KeyPath<Self, Value>,
                                 options: NSKeyValueObservingOptions = [.initial, .new])
        -> NSObject.KeyValueObservingPublisher<Self, Value> {
        return NSObject.KeyValueObservingPublisher(object: self, keyPath: keyPath, options: options)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension NSObject.KeyValueObservingPublisher {
    /// Returns a publisher that emits values when a KVO-compliant property changes.
    ///
    /// - Returns: A key-value observing publisher.
    public func didChange()
        -> Publishers.Map<NSObject.KeyValueObservingPublisher<Subject, Value>, Void> {
        return map { _ in () }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension NSObject {
    /// A publisher that emits events when the value of a KVO-compliant property changes.
    public struct KeyValueObservingPublisher<Subject: NSObject, Value> : Equatable {
        public let object: Subject
        public let keyPath: KeyPath<Subject, Value>
        public let options: NSKeyValueObservingOptions

        public init(
            object: Subject,
            keyPath: KeyPath<Subject, Value>,
            options: NSKeyValueObservingOptions
        ) {
            self.object = object
            self.keyPath = keyPath
            self.options = options
        }

        public static func == (
            lhs: KeyValueObservingPublisher,
            rhs: KeyValueObservingPublisher
        ) -> Bool {
            return lhs.object === rhs.object
                && lhs.keyPath == rhs.keyPath
                && lhs.options == rhs.options
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension NSObject.KeyValueObservingPublisher: Publisher {
    public typealias Output = Value
    public typealias Failure = Never

    public func receive<S: Subscriber>(subscriber: S) where S.Input == Output, S.Failure == Failure {
        let s = NSObject.KVOSubscription(object, keyPath, options, subscriber)
        subscriber.receive(subscription: s)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension NSObject {
    private final class KVOSubscription<Subject: NSObject, Value>: Subscription, CustomStringConvertible, CustomReflectable, CustomPlaygroundDisplayConvertible {
        private var observation: NSKeyValueObservation?         // GuardedBy(lock)
        private var demand: Subscribers.Demand                  // GuardedBy(lock)

        // for configurations that care about '.initial' we need to 'cache' the value to account for backpressure, along with whom to send it to
        //
        // TODO: in the future we might want to consider interjecting a temporary publisher that does this, so that all KVO subscriptions don't incur the cost.
        private var receivedInitial: Bool                       // GuardedBy(lock)
        private var last: Value?                                // GuardedBy(lock)
        private var subscriber: AnySubscriber<Value, Never>?    // GuardedBy(lock)

        private let lock = Lock()

        // This lock can only be held for the duration of downstream callouts
        private let downstreamLock = RecursiveLock()

        var description: String { return "KVOSubscription" }
        var customMirror: Mirror {
            lock.lock()
            defer { lock.unlock() }
            return Mirror(self, children: [
                "observation": observation as Any,
                "demand": demand
            ])
        }
        var playgroundDescription: Any { return description }

        init<S: Subscriber>(
            _ object: Subject,
            _ keyPath: KeyPath<Subject, Value>,
            _ options: NSKeyValueObservingOptions,
            _ subscriber: S)
            where
                S.Input == Value,
                S.Failure == Never
        {
            demand = .max(0)
            receivedInitial = false
            self.subscriber = AnySubscriber(subscriber)

            observation = object.observe(
                keyPath,
                options: options
            ) { [weak self] obj, _ in
                guard let self = self else {
                    return
                }
                let value = obj[keyPath: keyPath]
                self.lock.lock()
                if self.demand > 0, let sub = self.subscriber {
                    self.demand -= 1
                    self.lock.unlock()

                    self.downstreamLock.lock()
                    let additional = sub.receive(value)
                    self.downstreamLock.unlock()

                    self.lock.lock()
                    self.demand += additional
                    self.lock.unlock()
                } else {
                    // Drop the value, unless we've asked for .initial, and this
                    // is the first value.
                    if self.receivedInitial == false && options.contains(.initial) {
                        self.last = value
                        self.receivedInitial = true
                    }
                    self.lock.unlock()
                }
            }
        }

        deinit {
            lock.cleanupLock()
            downstreamLock.cleanupLock()
        }

        func request(_ d: Subscribers.Demand) {
            lock.lock()
            demand += d
            if demand > 0, let v = last, let sub = subscriber {
                demand -= 1
                last = nil
                lock.unlock()

                downstreamLock.lock()
                let additional = sub.receive(v)
                downstreamLock.unlock()

                lock.lock()
                demand += additional
            } else {
                demand -= 1
                last = nil
            }
            lock.unlock()
        }

        func cancel() {
            lock.lock()
            guard let o = observation else {
                lock.unlock()
                return
            }
            lock.unlock()

            observation = nil
            subscriber = nil
            last = nil
            o.invalidate()
        }
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
