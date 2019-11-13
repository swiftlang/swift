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

    /// Performs two-pole two-zero recursive filtering; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter coefficients: Filter coefficients.
    /// - Returns: Single-precision output vector.
    ///
    /// This function performs the following calculation:
    ///
    ///                for (n = 2; n < N+2; ++n)
    ///                    C[n] =
    ///                        + A[n-0]*B[0]
    ///                        + A[n-1]*B[1]
    ///                        + A[n-2]*B[2]
    ///                        - C[n-1]*B[3]
    ///                        - C[n-2]*B[4];
    ///
    /// Where `A` is the input vector, `B` is the filter coefficients, and `C`
    /// is the output vector. Note that outputs start with C[2].
    @inlinable
    public static func twoPoleTwoZeroFilter<U>(_ source: U,
                                                  coefficients: (Float, Float, Float, Float, Float)) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: source.count) {
                buffer, initializedCount in
                
                buffer[0] = 0
                buffer[1] = 0
                
                twoPoleTwoZeroFilter(source,
                                     coefficients: coefficients,
                                     result: &buffer)
                
                initializedCount = source.count
            }
            
            return result
    }
    
    /// Performs two-pole two-zero recursive filtering; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter coefficients: Filter coefficients.
    /// - Parameter result: Single-precision output vector.
    ///
    /// This function performs the following calculation:
    ///
    ///                for (n = 2; n < N+2; ++n)
    ///                    C[n] =
    ///                        + A[n-0]*B[0]
    ///                        + A[n-1]*B[1]
    ///                        + A[n-2]*B[2]
    ///                        - C[n-1]*B[3]
    ///                        - C[n-2]*B[4];
    ///
    /// Where `A` is the input vector, `B` is the filter coefficients, and `C`
    /// is the output vector. Note that outputs start with C[2].
    @inlinable
    public static func twoPoleTwoZeroFilter<U, V>(_ source: U,
                                                  coefficients: (Float, Float, Float, Float, Float),
                                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Float {
            
            precondition(source.count == result.count)
            
            let n = vDSP_Length(source.count - 2)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_deq22(src.baseAddress!, 1,
                               [coefficients.0, coefficients.1,
                                coefficients.2, coefficients.3,
                                coefficients.4],
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Performs two-pole two-zero recursive filtering; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter coefficients: Filter coefficients.
    /// - Returns: Double-precision output vector.
    ///
    /// This function performs the following calculation:
    ///
    ///                for (n = 2; n < N+2; ++n)
    ///                    C[n] =
    ///                        + A[n-0]*B[0]
    ///                        + A[n-1]*B[1]
    ///                        + A[n-2]*B[2]
    ///                        - C[n-1]*B[3]
    ///                        - C[n-2]*B[4];
    ///
    /// Where `A` is the input vector, `B` is the filter coefficients, and `C`
    /// is the output vector. Note that outputs start with C[2].
    @inlinable
    public static func twoPoleTwoZeroFilter<U>(_ source: U,
                                                  coefficients: (Double, Double, Double, Double, Double)) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: source.count) {
                buffer, initializedCount in
                
                buffer[0] = 0
                buffer[1] = 0
                
                twoPoleTwoZeroFilter(source,
                                     coefficients: coefficients,
                                     result: &buffer)
                
                initializedCount = source.count
            }
            
            return result
    }
    
    /// Performs two-pole two-zero recursive filtering; double-precision.
    ///
    /// - Parameter source: single-precision input vector.
    /// - Parameter coefficients: Filter coefficients.
    /// - Parameter result: single-precision output vector.
    ///
    /// This function performs the following calculation:
    ///
    ///                for (n = 2; n < N+2; ++n)
    ///                    C[n] =
    ///                        + A[n-0]*B[0]
    ///                        + A[n-1]*B[1]
    ///                        + A[n-2]*B[2]
    ///                        - C[n-1]*B[3]
    ///                        - C[n-2]*B[4];
    ///
    /// Where `A` is the input vector, `B` is the filter coefficients, and `C`
    /// is the output vector. Note that outputs start with C[2].
    @inlinable
    public static func twoPoleTwoZeroFilter<U, V>(_ source: U,
                                                  coefficients: (Double, Double, Double, Double, Double),
                                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Double {
            
            precondition(source.count == result.count)
            
            let n = vDSP_Length(source.count - 2)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_deq22D(src.baseAddress!, 1,
                                [coefficients.0, coefficients.1,
                                 coefficients.2, coefficients.3,
                                 coefficients.4],
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
}
