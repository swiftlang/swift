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
extension OperationQueue: Scheduler {
    /// The scheduler time type used by the operation queue.
    public struct SchedulerTimeType: Strideable, Codable, Hashable {
        /// The date represented by this type.
        public var date: Date
        
        /// Initializes a operation queue scheduler time with the given date.
        ///
        /// - Parameter date: The date to represent.
        public init(_ date: Date) {
            self.date = date
        }

        /// Returns the distance to another operation queue scheduler time.
        ///
        /// - Parameter other: Another operation queue time.
        /// - Returns: The time interval between this time and the provided time.
        public func distance(to other: OperationQueue.SchedulerTimeType) -> OperationQueue.SchedulerTimeType.Stride {
            return OperationQueue.SchedulerTimeType.Stride(floatLiteral: date.distance(to: other.date))
        }
    
        /// Returns a operation queue scheduler time calculated by advancing this instance’s time by the given interval.
        ///
        /// - Parameter n: A time interval to advance.
        /// - Returns: A operation queue time advanced by the given interval from this instance’s time.
        public func advanced(by n: OperationQueue.SchedulerTimeType.Stride) -> OperationQueue.SchedulerTimeType {
            return OperationQueue.SchedulerTimeType(date.advanced(by: n.timeInterval))
        }
        
        /// The interval by which operation queue times advance.
        public struct Stride: ExpressibleByFloatLiteral, Comparable, SignedNumeric, Codable, SchedulerTimeIntervalConvertible {
            public typealias FloatLiteralType = TimeInterval
            public typealias IntegerLiteralType = TimeInterval
            public typealias Magnitude = TimeInterval

            /// The value of this time interval in seconds.
            public var magnitude: TimeInterval
            
            /// The value of this time interval in seconds.
            public var timeInterval: TimeInterval {
                return magnitude
            }

            public init(integerLiteral value: TimeInterval) {
                magnitude = value
            }
            
            public init(floatLiteral value: TimeInterval) {
                magnitude = value
            }
            
            public init(_ timeInterval: TimeInterval) {
                magnitude = timeInterval
            }
            
            public init?<T>(exactly source: T) where T: BinaryInteger {
                if let d = TimeInterval(exactly: source) {
                    magnitude = d
                } else {
                    return nil
                }
            }
            
            // ---
            
            public static func < (lhs: Stride, rhs: Stride) -> Bool {
                return lhs.magnitude < rhs.magnitude
            }

            // ---
            
            public static func * (lhs: Stride, rhs: Stride) -> Stride {
                return Stride(lhs.timeInterval * rhs.timeInterval)
            }
            
            public static func + (lhs: Stride, rhs: Stride) -> Stride {
                return Stride(lhs.magnitude + rhs.magnitude)
            }
            
            public static func - (lhs: Stride, rhs: Stride) -> Stride {
                return Stride(lhs.magnitude - rhs.magnitude)
            }

            // ---
            
            public static func *= (lhs: inout Stride, rhs: Stride) {
                let result = lhs * rhs
                lhs = result
            }
            
            public static func += (lhs: inout Stride, rhs: Stride) {
                let result = lhs + rhs
                lhs = result
            }

            public static func -= (lhs: inout Stride, rhs: Stride) {
                let result = lhs - rhs
                lhs = result
            }
            
            // ---
            
            public static func seconds(_ s: Int) -> Stride {
                return Stride(Double(s))
            }
            
            public static func seconds(_ s: Double) -> Stride {
                return Stride(s)
            }
            
            public static func milliseconds(_ ms: Int) -> Stride {
                return Stride(Double(ms) / 1_000.0)
            }
            
            public static func microseconds(_ us: Int) -> Stride {
                return Stride(Double(us) / 1_000_000.0)
            }
            
            public static func nanoseconds(_ ns: Int) -> Stride {
                return Stride(Double(ns) / 1_000_000_000.0)
            }
        }
    }

    /// Options that affect the operation of the operation queue scheduler.
    public struct SchedulerOptions { }

    private final class DelayReadyOperation: Operation, Cancellable {
        static var readySchedulingQueue: DispatchQueue = {
            return DispatchQueue(label: "DelayReadyOperation")
        }()

        var action: (() -> Void)?
        var readyFromAfter: Bool

        init(_ action: @escaping() -> Void, after: OperationQueue.SchedulerTimeType) {
            self.action = action
            readyFromAfter = false
            super.init()
            let deadline = DispatchTime.now() + after.date.timeIntervalSinceNow            
            DelayReadyOperation.readySchedulingQueue.asyncAfter(deadline: deadline) { [weak self] in
                self?.becomeReady()
            }
        }
        
        override func main() {
            action!()
            action = nil
        }
        
        func becomeReady() {
            willChangeValue(for: \.isReady)
            readyFromAfter = true
            didChangeValue(for: \.isReady)
        }
        
        override var isReady: Bool {
            return super.isReady && readyFromAfter
        }
    }

    public func schedule(options: OperationQueue.SchedulerOptions?,
                         _ action: @escaping () -> Void) {
        let op = BlockOperation(block: action)
        addOperation(op)
    }

    public func schedule(after date: OperationQueue.SchedulerTimeType,
                         tolerance: OperationQueue.SchedulerTimeType.Stride,
                         options: OperationQueue.SchedulerOptions?,
                         _ action: @escaping () -> Void) {
        let op = DelayReadyOperation(action, after: date)
        addOperation(op)
    }
    
    public func schedule(after date: OperationQueue.SchedulerTimeType,
                         interval: OperationQueue.SchedulerTimeType.Stride,
                         tolerance: OperationQueue.SchedulerTimeType.Stride,
                         options: OperationQueue.SchedulerOptions?,
                         _ action: @escaping () -> Void) -> Cancellable {
        let op = DelayReadyOperation(action, after: date.advanced(by: interval))
        addOperation(op)
        return AnyCancellable(op)
    }

    public var now: OperationQueue.SchedulerTimeType {
        return OperationQueue.SchedulerTimeType(Date())
    }
    
    public var minimumTolerance: OperationQueue.SchedulerTimeType.Stride {
        return OperationQueue.SchedulerTimeType.Stride(0.0)
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
