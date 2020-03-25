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
extension RunLoop: Scheduler {
    /// The scheduler time type used by the run loop.
    public struct SchedulerTimeType: Strideable, Codable, Hashable {
        /// The date represented by this type.
        public var date: Date
        
        /// Initializes a run loop scheduler time with the given date.
        ///
        /// - Parameter date: The date to represent.
        public init(_ date: Date) {
            self.date = date
        }

        /// Returns the distance to another run loop scheduler time.
        ///
        /// - Parameter other: Another dispatch queue time.
        /// - Returns: The time interval between this time and the provided time.
        public func distance(to other: RunLoop.SchedulerTimeType) -> SchedulerTimeType.Stride {
            return Stride(floatLiteral: date.distance(to: other.date))
        }
    
        /// Returns a run loop scheduler time calculated by advancing this instance’s time by the given interval.
        ///
        /// - Parameter n: A time interval to advance.
        /// - Returns: A dispatch queue time advanced by the given interval from this instance’s time.
        public func advanced(by n: SchedulerTimeType.Stride) -> RunLoop.SchedulerTimeType {
            return SchedulerTimeType(date.advanced(by: n.timeInterval))
        }
        
        /// The interval by which run loop times advance.
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
    
    /// Options that affect the operation of the run loop scheduler.
    public struct SchedulerOptions { }

    public func schedule(options: SchedulerOptions?,
                         _ action: @escaping () -> Void) {
        self.perform(action)
    }
    
    public func schedule(after date: SchedulerTimeType,
                         tolerance: SchedulerTimeType.Stride,
                         options: SchedulerOptions?,
                         _ action: @escaping () -> Void) {
        let ti = date.date.timeIntervalSince(Date())
        self.perform(#selector(self.runLoopScheduled), with: _CombineRunLoopAction(action), afterDelay: ti)
    }
    
    public func schedule(after date: SchedulerTimeType,
                         interval: SchedulerTimeType.Stride,
                         tolerance: SchedulerTimeType.Stride,
                         options: SchedulerOptions?,
                         _ action: @escaping () -> Void) -> Cancellable {
        let timer = Timer(fire: date.date, interval: interval.timeInterval, repeats: true) { _ in
            action()
        }

        timer.tolerance = tolerance.timeInterval
        self.add(timer, forMode: .default)

        return AnyCancellable(timer.invalidate)
    }

    public var now: SchedulerTimeType {
        return SchedulerTimeType(Date())
    }
        
    public var minimumTolerance: SchedulerTimeType.Stride {
        return 0.0
    }
    
    @objc
    fileprivate func runLoopScheduled(action: _CombineRunLoopAction) {
        action.action()
    }
}

@objc
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
private class _CombineRunLoopAction: NSObject {
    let action: () -> Void
    
    init(_ action: @escaping () -> Void) {
        self.action = action
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
