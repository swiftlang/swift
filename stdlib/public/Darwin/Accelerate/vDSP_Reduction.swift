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
    
    // MARK: Maximum
    
    /// Returns vector maximum value; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The maximum value in `vector`.
    @inlinable
    public static func maximum<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxv(v.baseAddress!, 1,
                          &output,
                          n)
            }
            
            return output
    }
    
    /// Returns vector maximum value; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The maximum value in `vector`.
    @inlinable
    public static func maximum<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxvD(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    // MARK: Maximum magnitude
    
    /// Returns vector maximum magnitude; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The maximum magnitude in `vector`.
    @inlinable
    public static func maximumMagnitude<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxmgv(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
    /// Returns vector maximum magnitude; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The maximum magnitude in `vector`.
    @inlinable
    public static func maximumMagnitude<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxmgvD(v.baseAddress!, 1,
                             &output,
                             n)
            }
            
            return output
    }
    
    // MARK: Minimum
    
    /// Returns vector minimum value; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The minimum value in `vector`.
    @inlinable
    public static func minimum<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_minv(v.baseAddress!, 1,
                          &output,
                          n)
            }
            
            return output
    }
    
    /// Returns vector minimum value; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The minimum value in `vector`.
    @inlinable
    public static func minimum<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_minvD(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    // MARK: Summation
    
    /// Returns vector sum; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of values in `vector`.
    @inlinable
    public static func sum<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_sve(v.baseAddress!, 1,
                         &output,
                         n)
            }
            
            return output
    }
    
    /// Returns vector sum; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of values in `vector`.
    @inlinable
    public static func sum<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_sveD(v.baseAddress!, 1,
                          &output,
                          n)
            }
            
            return output
    }
    
    /// Returns vector sum of squares; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of squares in `vector`.
    @inlinable
    public static func sumOfSquares<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_svesq(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    /// Returns vector sum of squares; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of squares in `vector`.
    @inlinable
    public static func sumOfSquares<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_svesqD(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
    /// Returns sum of elements and sum of elements' squares; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of values and the sum of squares in `vector`.
    @inlinable
    public static func sumAndSumOfSquares<U>(_ vector: U) -> (elementsSum: Float, squaresSum: Float)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var sum = Float.nan
            var sumOfSquares = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_sve_svesq(v.baseAddress!, 1,
                               &sum,
                               &sumOfSquares,
                               n)
            }
            
            return (elementsSum: sum, squaresSum: sumOfSquares)
    }
    
    /// Returns sum of elements and sum of elements' squares; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of values and the sum of squares in `vector`.
    @inlinable
    public static func sumAndSumOfSquares<U>(_ vector: U) -> (elementsSum: Double, squaresSum: Double)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var sum = Double.nan
            var sumOfSquares = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_sve_svesqD(v.baseAddress!, 1,
                                &sum,
                                &sumOfSquares,
                                n)
            }
            
            return (elementsSum: sum, squaresSum: sumOfSquares)
    }
    
    /// Returns vector sum of magnitudes; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of magnitudes in `vector`.
    @inlinable
    public static func sumOfMagnitudes<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_svemg(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    /// Returns vector sum of magnitudes; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: The sum of magnitudes in `vector`.
    @inlinable
    public static func sumOfMagnitudes<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_svemgD(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Maximum with index
    
    /// Returns vector maximum with index; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the maximum value and its index.
    @inlinable
    public static func indexOfMaximum<U>(_ vector: U) -> (UInt, Float)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxvi(v.baseAddress!, 1,
                           &output,
                           &index,
                           n)
            }
            
            return (index, output)
    }
    
    /// Returns vector maximum with index; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the maximum value and its index.
    @inlinable
    public static func indexOfMaximum<U>(_ vector: U) -> (UInt, Double)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxviD(v.baseAddress!, 1,
                            &output,
                            &index,
                            n)
            }
            
            return (index, output)
    }
    
    // MARK: Maximum magnitude with index
    
    /// Returns vector maximum magnitude with index; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the maximum magnitude and its index.
    @inlinable
    public static func indexOfMaximumMagnitude<U>(_ vector: U) -> (UInt, Float)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxmgvi(v.baseAddress!, 1,
                             &output,
                             &index,
                             n)
            }
            
            return (index, output)
    }
    
    /// Returns vector maximum magnitude with index; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the maximum magnitude and its index.
    @inlinable
    public static func indexOfMaximumMagnitude<U>(_ vector: U) -> (UInt, Double)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_maxmgviD(v.baseAddress!, 1,
                              &output,
                              &index,
                              n)
            }
            
            return (index, output)
    }
    
    // MARK: Minimum with index
    
    /// Returns vector minimum with index; single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the minimum value and its index.
    @inlinable
    public static func indexOfMinimum<U>(_ vector: U) -> (UInt, Float)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_minvi(v.baseAddress!, 1,
                           &output,
                           &index,
                           n)
            }
            
            return (index, output)
    }
    
    /// Returns vector minimum with index; double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Returns: A tuple containing the minimum value and its index.
    @inlinable
    public static func indexOfMinimum<U>(_ vector: U) -> (UInt, Double)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            var index: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { v in
                vDSP_minviD(v.baseAddress!, 1,
                            &output,
                            &index,
                            n)
            }
            
            return (index, output)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Mean Square (vDSP_measqv)
    
    /// Returns the mean square of the supplied single-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func meanSquare<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_measqv(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
    /// Returns the mean square of the supplied double-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func meanSquare<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_measqvD(v.baseAddress!, 1,
                             &output,
                             n)
            }
            
            return output
    }
    
    // MARK: Mean Magnitude
    
    /// Returns the mean magnitude of the supplied single-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func meanMagnitude<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_meamgv(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
    /// Returns the mean magnitude of the supplied double-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func meanMagnitude<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_meamgvD(v.baseAddress!, 1,
                             &output,
                             n)
            }
            
            return output
    }
    
    
    // MARK: Mean
    
    /// Returns the mean magnitude of the supplied single-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func mean<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_meanv(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    /// Returns the mean of the supplied double-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func mean<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_meanvD(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
    // MARK: Root Mean Square
    
    /// Returns the root-mean-square of the supplied single-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func rootMeanSquare<U>(_ vector: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var output = Float.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_rmsqv(v.baseAddress!, 1,
                           &output,
                           n)
            }
            
            return output
    }
    
    /// Returns the root-mean-square of the supplied double-precision vector.
    ///
    /// - Parameter vector: The input vector.
    @inlinable
    public static func rootMeanSquare<U>(_ vector: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var output = Double.nan
            
            vector.withUnsafeBufferPointer { v in
                vDSP_rmsqvD(v.baseAddress!, 1,
                            &output,
                            n)
            }
            
            return output
    }
    
}
