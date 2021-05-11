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

import Combine

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
private func clampedIntProduct(_ m1: Int, _ m2: UInt64) -> Int {
    assert(m2 > 0, "multiplier must be positive")
    guard m1 < Int.max, m2 < Int.max else { return Int.max }
    let (result, overflow) = m1.multipliedReportingOverflow(by: Int(m2))
    if overflow {
        return m1 > 0 ? Int.max : Int.min
    }
    return result
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DispatchTimeInterval {    
    fileprivate var nanoseconds: Int {
        switch self {
        case .seconds(let s): return clampedIntProduct(s, NSEC_PER_SEC)
        case .milliseconds(let ms): return clampedIntProduct(ms, NSEC_PER_MSEC)
        case .microseconds(let us): return clampedIntProduct(us, NSEC_PER_USEC)
        case .nanoseconds(let ns): return ns
        case .never: return Int.max
        }
    }
}

// This is Strideable except: <rdar://problem/35158274>
@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DispatchTime /* : Strideable */ {
    typealias Stride = DispatchTimeInterval
    
    public func distance(to other: DispatchTime) -> DispatchTimeInterval {
        let lhs = other.uptimeNanoseconds
        let rhs = uptimeNanoseconds
        if lhs >= rhs {
            return DispatchTimeInterval.nanoseconds(Int(lhs - rhs))
        } else {
            return DispatchTimeInterval.nanoseconds(0 - Int(rhs - lhs))
        }
    }
    
    public func advanced(by n: DispatchTimeInterval) -> DispatchTime {
        return self + n
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DispatchQueue: Scheduler {
    /// The scheduler time type used by the dispatch queue.
    public struct SchedulerTimeType: Strideable, Codable, Hashable {
        /// The dispatch time represented by this type.
        public var dispatchTime: DispatchTime
        
        /// Creates a dispatch queue time type instance.
        ///
        /// - Parameter time: The dispatch time to represent.
        public init(_ time: DispatchTime) {
            dispatchTime = time
        }

        public init(from decoder: Decoder) throws {
            let container = try decoder.singleValueContainer()
            let time = DispatchTime(uptimeNanoseconds: try container.decode(UInt64.self))
            self.init(time)
        }

        public func encode(to encoder: Encoder) throws {
            var container = encoder.singleValueContainer()
            try container.encode(dispatchTime.uptimeNanoseconds)
        }
        
        /// Returns the distance to another dispatch queue time.
        ///
        /// - Parameter other: Another dispatch queue time.
        /// - Returns: The time interval between this time and the provided time.
        public func distance(to other: SchedulerTimeType) -> Stride {
            return Stride(self.dispatchTime.distance(to: other.dispatchTime))
        }
        
        /// Returns a dispatch queue scheduler time calculated by advancing this instance’s time by the given interval.
        ///
        /// - Parameter n: A time interval to advance.
        /// - Returns: A dispatch queue time advanced by the given interval from this instance’s time.
        public func advanced(by n: Stride) -> SchedulerTimeType {
            return SchedulerTimeType(self.dispatchTime.advanced(by: n.timeInterval))
        }
        
        public func hash(into hasher: inout Hasher) {
            hasher.combine(dispatchTime.rawValue)
        }
        
        public struct Stride: SchedulerTimeIntervalConvertible, Comparable, SignedNumeric, ExpressibleByFloatLiteral, Hashable, Codable {
            /// If created via floating point literal, the value is converted to nanoseconds via multiplication.
            public typealias FloatLiteralType = Double
            
            /// Nanoseconds, same as DispatchTimeInterval.
            public typealias IntegerLiteralType = Int
            public typealias Magnitude = Int
            
            /// The value of this time interval in nanoseconds.
            public var magnitude: Int
            
            /// A `DispatchTimeInterval` created with the value of this type in nanoseconds.
            public var timeInterval: DispatchTimeInterval {
                return .nanoseconds(magnitude)
            }
            
            /// Creates a dispatch queue time interval from the given dispatch time interval.
            ///
            /// - Parameter timeInterval: A dispatch time interval.
            public init(_ timeInterval: DispatchTimeInterval) {
                magnitude = Int(timeInterval.nanoseconds)
            }

            /// Creates a dispatch queue time interval from a floating-point seconds value.
            ///
            /// - Parameter value: The number of seconds, as a `Double`.
            public init(floatLiteral value: Double) {
                magnitude = Int(value * 1_000_000_000)
            }

            /// Creates a dispatch queue time interval from an integer seconds value.
            ///
            /// - Parameter value: The number of seconds, as an `Int`.
            public init(integerLiteral value: Int) {
                magnitude = value * 1_000_000_000
            }
            
            /// Creates a dispatch queue time interval from a binary integer type.
            ///
            /// If `exactly` cannot convert to an `Int`, the resulting time interval is `nil`.
            /// - Parameter exactly: A binary integer representing a time interval.
            public init?<T>(exactly source: T) where T: BinaryInteger {
                if let v = Int(exactly: source) {
                    magnitude = v
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
                return Stride(.nanoseconds(lhs.magnitude * rhs.magnitude))
            }
            
            public static func + (lhs: Stride, rhs: Stride) -> Stride {
                return Stride(.nanoseconds(lhs.magnitude + rhs.magnitude))
            }
            
            public static func - (lhs: Stride, rhs: Stride) -> Stride {
                return Stride(.nanoseconds(lhs.magnitude - rhs.magnitude))
            }

            // ---
            
            public static func -= (lhs: inout Stride, rhs: Stride) {
                let result = lhs - rhs
                lhs = result
            }
            
            public static func *= (lhs: inout Stride, rhs: Stride) {
                let result = lhs * rhs
                lhs = result
            }
            
            public static func += (lhs: inout Stride, rhs: Stride) {
                let result = lhs + rhs
                lhs = result
            }
           
            // ---
            
            public static func seconds(_ s: Double) -> Stride {
                return Stride(.nanoseconds(Int(s * 1_000_000_000)))
            }
            
            public static func seconds(_ s: Int) -> Stride {
                return Stride(.seconds(s))
            }
            
            public static func milliseconds(_ ms: Int) -> Stride {
                return Stride(.milliseconds(ms))
            }
            
            public static func microseconds(_ us: Int) -> Stride {
                return Stride(.microseconds(us))
            }
            
            public static func nanoseconds(_ ns: Int) -> Stride {
                return Stride(.nanoseconds(ns))
            }
        }
    }
    
    /// Options that affect the operation of the dispatch queue scheduler.
    public struct SchedulerOptions {
        /// The dispatch queue quality of service.
        public var qos: DispatchQoS

        /// The dispatch queue work item flags.
        public var flags: DispatchWorkItemFlags
        
        /// The dispatch group, if any, that should be used for performing actions.
        public var group: DispatchGroup?
        
        public init(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], group: DispatchGroup? = nil) {
            self.qos = qos
            self.flags = flags
            self.group = group
        }
    }
    
    public var minimumTolerance: SchedulerTimeType.Stride {
        return SchedulerTimeType.Stride(DispatchTimeInterval.seconds(0))
    }
    
    public var now: DispatchQueue.SchedulerTimeType {
        return SchedulerTimeType(DispatchTime.now())
    }

    public func schedule(options: SchedulerOptions?, _ action: @escaping () -> Void) {
        let qos = options?.qos ?? .unspecified
        let flags = options?.flags ?? []
        
        if let group = options?.group {
            // Distinguish on the group because it appears to not be a call-through like the others. This may need to be adjusted.
            self.async(group: group, qos: qos, flags: flags, execute: action)
        } else {
            self.async(qos: qos, flags: flags, execute: action)
        }
    }
    
    public func schedule(after date: SchedulerTimeType,
                         tolerance: SchedulerTimeType.Stride,
                         options: SchedulerOptions?,
                         _ action: @escaping () -> Void) {
        // TODO: Tolerance ignored
        let qos = options?.qos ?? .unspecified
        let flags = options?.flags ?? []
        
        self.asyncAfter(deadline: date.dispatchTime, qos: qos, flags: flags, execute: action)
    }
    
    public func schedule(after date: SchedulerTimeType,
                         interval: SchedulerTimeType.Stride,
                         tolerance: SchedulerTimeType.Stride,
                         options: SchedulerOptions?,
                         _ action: @escaping () -> Void) -> Cancellable {
        let source = DispatchSource.makeTimerSource(flags: DispatchSource.TimerFlags(), queue: self)
        
        source.schedule(deadline: date.dispatchTime,
                        repeating: interval.timeInterval,
                        leeway: tolerance.timeInterval)
        source.setEventHandler(handler: action)
        source.resume()
        
        return AnyCancellable(source.cancel)
    }
}

#endif /* !(os(iOS) && (arch(i386) || arch(arm))) */
