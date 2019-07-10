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

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    /// Vector sliding window sum; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter windowLength: The number of consecutive elements to sum.
    /// - Returns: Single-precision output vector.
    @inlinable
    public static func slidingWindowSum<U>(_ vector: U,
                                           usingWindowLength windowLength: Int) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vector.count - windowLength + 1
            
            let result = Array<Float>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                slidingWindowSum(vector,
                                 usingWindowLength: windowLength,
                                 result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// Vector sliding window sum; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter windowLength: The number of consecutive elements to sum.
    /// - Parameter result: Single-precision output vector.
    @inlinable
    public static func slidingWindowSum<U, V>(_ vector: U,
                                              usingWindowLength windowLength: Int,
                                              result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n + windowLength - 1)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vswsum(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                vDSP_Length(n),
                                vDSP_Length(windowLength))
                }
            }
            
    }
    
    /// Vector sliding window sum; double-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter windowLength: The number of consecutive elements to sum.
    /// - Returns: Single-precision output vector.
    @inlinable
    public static func slidingWindowSum<U>(_ vector: U,
                                           usingWindowLength windowLength: Int) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vector.count - windowLength + 1
            
            let result = Array<Double>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                slidingWindowSum(vector,
                                 usingWindowLength: windowLength,
                                 result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// Vector sliding window sum; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter windowLength: The number of consecutive elements to sum.
    /// - Parameter result: Double-precision output vector.
    @inlinable
    public static func slidingWindowSum<U, V>(_ vector: U,
                                              usingWindowLength windowLength: Int,
                                              result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n + windowLength - 1)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vswsumD(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 vDSP_Length(n),
                                 vDSP_Length(windowLength))
                }
            }
            
    }
}
