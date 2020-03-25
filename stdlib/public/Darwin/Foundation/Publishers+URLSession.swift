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

// MARK: Data Tasks

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension URLSession {
    /// Returns a publisher that wraps a URL session data task for a given URL.
    ///
    /// The publisher publishes data when the task completes, or terminates if the task fails with an error.
    /// - Parameter url: The URL for which to create a data task.
    /// - Returns: A publisher that wraps a data task for the URL.
    public func dataTaskPublisher(
        for url: URL)
        -> DataTaskPublisher
    {
        let request = URLRequest(url: url)
        return DataTaskPublisher(request: request, session: self)
    }

    /// Returns a publisher that wraps a URL session data task for a given URL request.
    ///
    /// The publisher publishes data when the task completes, or terminates if the task fails with an error.
    /// - Parameter request: The URL request for which to create a data task.
    /// - Returns: A publisher that wraps a data task for the URL request.
    public func dataTaskPublisher(
        for request: URLRequest)
        -> DataTaskPublisher
    {
        return DataTaskPublisher(request: request, session: self)
    }

    public struct DataTaskPublisher: Publisher {
        public typealias Output = (data: Data, response: URLResponse)
        public typealias Failure = URLError
        
        public let request: URLRequest
        public let session: URLSession
        
        public init(request: URLRequest, session: URLSession) {
            self.request = request
            self.session = session
        }
        
        public func receive<S: Subscriber>(subscriber: S) where Failure == S.Failure, Output == S.Input {
            subscriber.receive(subscription: Inner(self, subscriber))
        }
        
        private typealias Parent = DataTaskPublisher
        private final class Inner<Downstream: Subscriber>: Subscription, CustomStringConvertible, CustomReflectable, CustomPlaygroundDisplayConvertible
            where
                Downstream.Input == Parent.Output,
                Downstream.Failure == Parent.Failure
        {
            typealias Input = Downstream.Input
            typealias Failure = Downstream.Failure
            
            private let lock: Lock
            private var parent: Parent?             // GuardedBy(lock)
            private var downstream: Downstream?     // GuardedBy(lock)
            private var demand: Subscribers.Demand  // GuardedBy(lock)
            private var task: URLSessionDataTask!   // GuardedBy(lock)

            var description: String { return "DataTaskPublisher" }
            var customMirror: Mirror {
                lock.lock()
                defer { lock.unlock() }
                return Mirror(self, children: [
                    "task": task as Any,
                    "downstream": downstream as Any,
                    "parent": parent as Any,
                    "demand": demand,
                ])
            }
            var playgroundDescription: Any { return description }
            
            init(_ parent: Parent, _ downstream: Downstream) {
                self.lock = Lock()
                self.parent = parent
                self.downstream = downstream
                self.demand = .max(0)
            }
            
            deinit {
                lock.cleanupLock()
            }
            
            // MARK: - Upward Signals
            func request(_ d: Subscribers.Demand) {
                precondition(d > 0, "Invalid request of zero demand")
                
                lock.lock()
                guard let p = parent else {
                    // We've already been cancelled so bail
                    lock.unlock()
                    return
                }
                
                // Avoid issues around `self` before init by setting up only once here
                if self.task == nil {
                    let task = p.session.dataTask(
                        with: p.request,
                        completionHandler: handleResponse(data:response:error:)
                    )
                    self.task = task
                }
                
                self.demand += d
                let task = self.task!
                lock.unlock()
                
                task.resume()
            }
            
            private func handleResponse(data: Data?, response: URLResponse?, error: Error?) {
                lock.lock()
                guard demand > 0,
                      parent != nil,
                      let ds = downstream
                else {
                    lock.unlock()
                    return
                }
                
                parent = nil
                downstream = nil

                // We clear demand since this is a single shot shape
                demand = .max(0)
                task = nil
                lock.unlock()
                
                if let response = response, error == nil {
                    _ = ds.receive((data ?? Data(), response))
                    ds.receive(completion: .finished)
                } else {
                    let urlError = error as? URLError ?? URLError(.unknown)
                    ds.receive(completion: .failure(urlError))
                }
            }
            
            func cancel() {
                lock.lock()
                guard parent != nil else {
                    lock.unlock()
                    return
                }
                parent = nil
                downstream = nil
                demand = .max(0)
                let task = self.task
                self.task = nil
                lock.unlock()
                task?.cancel()
            }
        }
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
