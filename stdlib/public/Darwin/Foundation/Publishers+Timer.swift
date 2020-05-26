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

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Timer {
    /// Returns a publisher that repeatedly emits the current date on the given interval.
    ///
    /// - Parameters:
    ///   - interval: The time interval on which to publish events. For example, a value of `0.5` publishes an event approximately every half-second.
    ///   - tolerance: The allowed timing variance when emitting events. Defaults to `nil`, which allows any variance.
    ///   - runLoop: The run loop on which the timer runs.
    ///   - mode: The run loop mode in which to run the timer.
    ///   - options: Scheduler options passed to the timer. Defaults to `nil`.
    /// - Returns: A publisher that repeatedly emits the current date on the given interval.
    public static func publish(
        every interval: TimeInterval,
        tolerance: TimeInterval? = nil,
        on runLoop: RunLoop,
        in mode: RunLoop.Mode,
        options: RunLoop.SchedulerOptions? = nil)
        -> TimerPublisher
    {
        return TimerPublisher(interval: interval, runLoop: runLoop, mode: mode, options: options)
    }
    
    /// A publisher that repeatedly emits the current date on a given interval.
    public final class TimerPublisher: ConnectablePublisher {
        public typealias Output = Date
        public typealias Failure = Never

        public let interval: TimeInterval
        public let tolerance: TimeInterval?
        public let runLoop: RunLoop
        public let mode: RunLoop.Mode
        public let options: RunLoop.SchedulerOptions?

        private lazy var routingSubscription: RoutingSubscription = {
            return RoutingSubscription(parent: self)
        }()
        
        // Stores if a `.connect()` happened before subscription, internally readable for tests
        internal var isConnected: Bool {
            return routingSubscription.isConnected
        }
        
        /// Creates a publisher that repeatedly emits the current date on the given interval.
        ///
        /// - Parameters:
        ///   - interval: The interval on which to publish events.
        ///   - tolerance: The allowed timing variance when emitting events. Defaults to `nil`, which allows any variance.
        ///   - runLoop: The run loop on which the timer runs.
        ///   - mode: The run loop mode in which to run the timer.
        ///   - options: Scheduler options passed to the timer. Defaults to `nil`.
        public init(interval: TimeInterval, tolerance: TimeInterval? = nil, runLoop: RunLoop, mode: RunLoop.Mode, options: RunLoop.SchedulerOptions? = nil) {
            self.interval = interval
            self.tolerance = tolerance
            self.runLoop = runLoop
            self.mode = mode
            self.options = options
        }

        /// Adapter subscription to allow `Timer` to multiplex to multiple subscribers
        /// the values produced by a single `TimerPublisher.Inner`
        private class RoutingSubscription: Subscription, Subscriber, CustomStringConvertible, CustomReflectable, CustomPlaygroundDisplayConvertible {
            typealias Input = Date
            typealias Failure = Never

            private typealias ErasedSubscriber = AnySubscriber<Output, Failure>

            private let lock: Lock

            // Inner is IUP due to init requirements
            private var inner: Inner<RoutingSubscription>!
            private var subscribers: [ErasedSubscriber] = []

            private var _lockedIsConnected = false
            var isConnected: Bool {
                get {
                    lock.lock()
                    defer { lock.unlock() }
                    return _lockedIsConnected
                }

                set {
                    lock.lock()
                    let oldValue = _lockedIsConnected
                    _lockedIsConnected = newValue

                    // Inner will always be non-nil
                    let inner = self.inner!
                    lock.unlock()

                    guard newValue, !oldValue else {
                        return
                    }
                    inner.enqueue()
                }
            }
            
            var description: String { return "Timer" }
            var customMirror: Mirror { return inner.customMirror }
            var playgroundDescription: Any { return description }
            var combineIdentifier: CombineIdentifier { return inner.combineIdentifier }
            
            init(parent: TimerPublisher) {
                self.lock = Lock()
                self.inner = Inner(parent, self)
            }

            deinit {
                lock.cleanupLock()
            }

            func addSubscriber<S: Subscriber>(_ sub: S)
                where
                    S.Failure == Failure,
                    S.Input == Output
            {
                lock.lock()
                subscribers.append(AnySubscriber(sub))
                lock.unlock()

                sub.receive(subscription: self)
            }

            func receive(subscription: Subscription) {
                lock.lock()
                let subscribers = self.subscribers
                lock.unlock()

                for sub in subscribers {
                    sub.receive(subscription: subscription)
                }
            }

            func receive(_ value: Input) -> Subscribers.Demand {
                var resultingDemand: Subscribers.Demand = .max(0)
                lock.lock()
                let subscribers = self.subscribers
                let isConnected = _lockedIsConnected
                lock.unlock()

                guard isConnected else { return .none }

                for sub in subscribers {
                    resultingDemand += sub.receive(value)
                }
                return resultingDemand
            }

            func receive(completion: Subscribers.Completion<Failure>) {
                lock.lock()
                let subscribers = self.subscribers
                lock.unlock()

                for sub in subscribers {
                    sub.receive(completion: completion)
                }
            }

            func request(_ demand: Subscribers.Demand) {
                lock.lock()
                // Inner will always be non-nil
                let inner = self.inner!
                lock.unlock()

                inner.request(demand)
            }

            func cancel() {
                lock.lock()
                // Inner will always be non-nil
                let inner = self.inner!
                _lockedIsConnected = false
                self.subscribers = []
                lock.unlock()

                inner.cancel()
            }
        }

        public func receive<S: Subscriber>(subscriber: S) where Failure == S.Failure, Output == S.Input {
            routingSubscription.addSubscriber(subscriber)
        }

        public func connect() -> Cancellable {
            routingSubscription.isConnected = true
            return routingSubscription
        }

        private typealias Parent = TimerPublisher
        private final class Inner<Downstream: Subscriber>: NSObject, Subscription, CustomReflectable, CustomPlaygroundDisplayConvertible
            where
                Downstream.Input == Date,
                Downstream.Failure == Never
        {
            private lazy var timer: Timer? = {
                let t = Timer(
                    timeInterval: parent?.interval ?? 0,
                    target: self,
                    selector: #selector(timerFired),
                    userInfo: nil,
                    repeats: true
                )

                t.tolerance = parent?.tolerance ?? 0

                return t
            }()

            private let lock: Lock
            private var downstream: Downstream?
            private var parent: Parent?
            private var started: Bool
            private var demand: Subscribers.Demand
            
            override var description: String { return "Timer" }
            var customMirror: Mirror {
                lock.lock()
                defer { lock.unlock() }
                return Mirror(self, children: [
                    "downstream": downstream as Any,
                    "interval": parent?.interval as Any,
                    "tolerance": parent?.tolerance as Any
                ])
            }
            var playgroundDescription: Any { return description }
            
            init(_ parent: Parent, _ downstream: Downstream) {
                self.lock = Lock()
                self.parent = parent
                self.downstream = downstream
                self.started = false
                self.demand = .max(0)
                super.init()
            }

            deinit {
                lock.cleanupLock()
            }

            func enqueue() {
                lock.lock()
                guard let t = timer, let parent = self.parent, !started else {
                    lock.unlock()
                    return
                }

                started = true
                lock.unlock()
                
                parent.runLoop.add(t, forMode: parent.mode)
            }
            
            func cancel() {
                lock.lock()
                guard let t = timer else {
                    lock.unlock()
                    return
                }

                // clear out all optionals
                downstream = nil
                parent = nil
                started = false
                demand = .max(0)
                timer = nil
                lock.unlock()

                // cancel the timer
                t.invalidate()
            }
            
            func request(_ n: Subscribers.Demand) {
                lock.lock()
                defer { lock.unlock() }
                guard parent != nil else {
                    return
                }
                demand += n
            }

            @objc
            func timerFired(arg: Any) {
                lock.lock()
                guard let ds = downstream, parent != nil else {
                    lock.unlock()
                    return
                }

                // This publisher drops events on the floor when there is no space in the subscriber
                guard demand > 0 else {
                    lock.unlock()
                    return
                }

                demand -= 1
                lock.unlock()
                
                let extra = ds.receive(Date())
                guard extra > 0 else {
                    return
                }

                lock.lock()
                demand += extra
                lock.unlock()
            }
        }
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
