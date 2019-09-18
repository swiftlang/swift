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
    
    /// FIR filtering with decimation and antialiasing; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Returns: Single-precision output vector.
    @inlinable
    public static func downsample<T, U>(_ source: U,
                                        decimationFactor: Int,
                                        filter: T) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float,
        U.Element == Float {
            
            let n = (source.count - filter.count) / decimationFactor + 1
            
            let result = Array<Float>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                downsample(source,
                           decimationFactor: decimationFactor,
                           filter: filter,
                           result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// FIR filtering with decimation and antialiasing; single-precision.
    ///
    /// - Parameter source: Single-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Single-precision output vector.
    @inlinable
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFactor: Int,
                                           filter: T,
                                           result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float,
        U.Element == Float,
        V.Element == Float {
            
            let p = filter.count
            let n = result.count
            
            precondition(source.count == decimationFactor * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desamp(src.baseAddress!,
                                    vDSP_Stride(decimationFactor),
                                    f.baseAddress!,
                                    dest.baseAddress!,
                                    vDSP_Length(n),
                                    vDSP_Length(p))
                    }
                }
            }
    }
    
    /// FIR filtering with decimation and antialiasing; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Returns: Double-precision output vector.
    @inlinable
    public static func downsample<T, U>(_ source: U,
                                        decimationFactor: Int,
                                        filter: T) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double,
        U.Element == Double {
            let n = (source.count - filter.count) / decimationFactor + 1
            
            let result = Array<Double>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                downsample(source,
                           decimationFactor: decimationFactor,
                           filter: filter,
                           result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// FIR filtering with decimation and antialiasing; double-precision.
    ///
    /// - Parameter source: Double-precision input vector.
    /// - Parameter decimationFactor: The integer factor by which to divide the sampling rate.
    /// - Parameter filter: Filter to use during the downsampling operation.
    /// - Parameter result: Double-precision output vector.
    @inlinable
    public static func downsample<T, U, V>(_ source: U,
                                           decimationFactor: Int,
                                           filter: T,
                                           result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double,
        U.Element == Double,
        V.Element == Double {
            
            let p = filter.count
            let n = result.count
            
            precondition(source.count == decimationFactor * (n - 1) + p)
            
            result.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    filter.withUnsafeBufferPointer { f in
                        
                        vDSP_desampD(src.baseAddress!,
                                     vDSP_Stride(decimationFactor),
                                     f.baseAddress!,
                                     dest.baseAddress!,
                                     vDSP_Length(n),
                                     vDSP_Length(p))
                    }
                }
            }
    }
    
}
