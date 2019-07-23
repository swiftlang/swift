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
    
    // MARK: One-dimensional convolution
    
    /// Returns one-dimensional convolution, single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      withKernel kernel: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let n = vector.count - kernel.count
            
            let result = Array<Float>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                convolve(vector,
                         withKernel: kernel,
                         result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// One-dimensional convolution, single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         withKernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count >= n + kernel.count - 1,
                         "Source vector count must be at least the sum of the result and kernel counts, minus one.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_conv(src.baseAddress!, 1,
                                  k.baseAddress!.advanced(by: kernel.count - 1), -1,
                                  dest.baseAddress!, 1,
                                  vDSP_Length(n),
                                  vDSP_Length(kernel.count))
                    }
                }
            }
    }
    
    /// Returns one-dimensional convolution, double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter kernel: Double-precision convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      withKernel kernel: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let n = vector.count - kernel.count
            
            let result = Array<Double>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                convolve(vector,
                         withKernel: kernel,
                         result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// One-dimensional convolution, double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter kernel: Double-precision convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         withKernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count >= n + kernel.count - 1,
                         "Source vector count must be at least the sum of the result and kernel counts, minus one.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_convD(src.baseAddress!, 1,
                                   k.baseAddress!.advanced(by: kernel.count - 1), -1,
                                   dest.baseAddress!, 1,
                                   vDSP_Length(n),
                                   vDSP_Length(kernel.count))
                    }
                }
            }
    }
    
    // MARK: One-dimensional correlation
    
    /// Returns one-dimensional correlation, single-precision.
    ///
    /// - Parameter vector: The vector to correlate.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Returns: Correlation result.
    @inlinable
    public static func correlate<T, U>(_ vector: T,
                                       withKernel kernel: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let n = vector.count - kernel.count
            
            let result = Array<Float>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                correlate(vector,
                          withKernel: kernel,
                          result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// One-dimensional correlation, single-precision.
    ///
    /// - Parameter vector: The vector to correlate.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func correlate<T, U, V>(_ vector: T,
                                          withKernel kernel: U,
                                          result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count >= n + kernel.count - 1,
                         "Source vector count must be at least the sum of the result and kernel counts, minus one.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_conv(src.baseAddress!, 1,
                                  k.baseAddress!, 1,
                                  dest.baseAddress!, 1,
                                  vDSP_Length(n),
                                  vDSP_Length(kernel.count))
                    }
                }
            }
    }
    
    /// Returns one-dimensional correlation, double-precision.
    ///
    /// - Parameter vector: The vector to correlate.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Returns: Correlation result.
    @inlinable
    public static func correlate<T, U>(_ vector: T,
                                       withKernel kernel: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let n = vector.count - kernel.count
            
            let result = Array<Double>(unsafeUninitializedCapacity: n) {
                buffer, initializedCount in
                
                correlate(vector,
                          withKernel: kernel,
                          result: &buffer)
                
                initializedCount = n
            }
            
            return result
    }
    
    /// One-dimensional correlation, double-precision.
    ///
    /// - Parameter vector: The vector to correlate.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func correlate<T, U, V>(_ vector: T,
                                          withKernel kernel: U,
                                          result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count >= n + kernel.count - 1,
                         "Source vector count must be at least the sum of the result and kernel counts, minus one.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_convD(src.baseAddress!, 1,
                                   k.baseAddress!, 1,
                                   dest.baseAddress!, 1,
                                   vDSP_Length(n),
                                   vDSP_Length(kernel.count))
                    }
                }
            }
    }
    
}


@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Two-dimensional convolution
    
    /// Returns two-dimensional convolution with a 3 x 3 kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision 3x3 convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      with3x3Kernel kernel: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         with3x3Kernel: kernel,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with a 3 x 3 kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision 3x3 convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         with3x3Kernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernel.count == 9,
                         "Kernel must contain 9 elements.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_f3x3(src.baseAddress!,
                                  vDSP_Length(rowCount), vDSP_Length(columnCount),
                                  k.baseAddress!,
                                  dest.baseAddress!)
                    }
                }
            }
    }
    
    /// Returns two-dimensional convolution with a 3 x 3 kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Double-precision 3x3 convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      with3x3Kernel kernel: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         with3x3Kernel: kernel,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with a 3 x 3 kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Double-precision 3x3 convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         with3x3Kernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernel.count == 9,
                         "Kernel must contain 9 elements.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_f3x3D(src.baseAddress!,
                                   vDSP_Length(rowCount), vDSP_Length(columnCount),
                                   k.baseAddress!,
                                   dest.baseAddress!)
                    }
                }
            }
    }
    
    /// Returns two-dimensional convolution with a 5 x 5 kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision 5x5 convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      with5x5Kernel kernel: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         with5x5Kernel: kernel,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with a 5 x 5 kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision 5x5 convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         with5x5Kernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernel.count == 25,
                         "Kernel must contain 25 elements.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_f5x5(src.baseAddress!,
                                  vDSP_Length(rowCount), vDSP_Length(columnCount),
                                  k.baseAddress!,
                                  dest.baseAddress!)
                    }
                }
            }
    }
    
    /// Returns two-dimensional convolution with a 5 x 5 kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Double-precision 3x3 convolution kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      with5x5Kernel kernel: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         with5x5Kernel: kernel,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with a 5 x 5 kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Double-precision 5x5 convolution kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         with5x5Kernel kernel: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernel.count == 25,
                         "Kernel must contain 25 elements.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_f5x5D(src.baseAddress!,
                                   vDSP_Length(rowCount), vDSP_Length(columnCount),
                                   k.baseAddress!,
                                   dest.baseAddress!)
                    }
                }
            }
    }
    
    /// Returns two-dimensional convolution with arbitrarily sized kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter kernelRowCount: The number of rows in the kernel.
    /// - Parameter kernelColumnCount: The number of columns in the kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      withKernel kernel: U,
                                      kernelRowCount: Int, kernelColumnCount: Int) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         withKernel: kernel,
                         kernelRowCount: kernelRowCount, kernelColumnCount: kernelColumnCount,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with arbitrarily sized kernel; single-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter kernelRowCount: The number of rows in the kernel.
    /// - Parameter kernelColumnCount: The number of columns in the kernel.
    /// - Parameter result: Destination vector.
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         withKernel kernel: U,
                                         kernelRowCount: Int, kernelColumnCount: Int,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernelRowCount % 2 == 1,
                         "Kernel row count must be odd.")
            
            precondition(kernelColumnCount % 2 == 1,
                         "Kernel column count must be odd.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_imgfir(src.baseAddress!,
                                    vDSP_Length(rowCount), vDSP_Length(columnCount),
                                    k.baseAddress!,
                                    dest.baseAddress!,
                                    vDSP_Length(kernelRowCount), vDSP_Length(kernelColumnCount))
                    }
                }
            }
    }
    
    /// Returns two-dimensional convolution with arbitrarily sized kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Single-precision convolution kernel.
    /// - Parameter kernelRowCount: The number of rows in the kernel.
    /// - Parameter kernelColumnCount: The number of columns in the kernel.
    /// - Returns: Convolution result.
    @inlinable
    public static func convolve<T, U>(_ vector: T,
                                      rowCount: Int, columnCount: Int,
                                      withKernel kernel: U,
                                      kernelRowCount: Int, kernelColumnCount: Int) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                convolve(vector,
                         rowCount: rowCount, columnCount: columnCount,
                         withKernel: kernel,
                         kernelRowCount: kernelRowCount, kernelColumnCount: kernelColumnCount,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Two-dimensional convolution with arbitrarily sized kernel; double-precision.
    ///
    /// - Parameter vector: The vector to convolve.
    /// - Parameter rowCount: The number of rows in the input vector.
    /// - Parameter columnCount: The number of columns in the input vector.
    /// - Parameter kernel: Double-precision convolution kernel.
    /// - Parameter kernelRowCount: The number of rows in the kernel.
    /// - Parameter kernelColumnCount: The number of columns in the kernel.
    /// - Parameter result: Destination vector.
    
    @inlinable
    public static func convolve<T, U, V>(_ vector: T,
                                         rowCount: Int, columnCount: Int,
                                         withKernel kernel: U,
                                         kernelRowCount: Int, kernelColumnCount: Int,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(rowCount >= 3,
                         "Row count must be greater than or equal to 3.")
            
            precondition(columnCount >= 4,
                         "Column count must be even and greater than or equal to 4")
            
            precondition(rowCount * columnCount == vector.count,
                         "Row count `x` column count must equal source vector count.")
            
            precondition(kernelRowCount % 2 == 1,
                         "Kernel row count must be odd.")
            
            precondition(kernelColumnCount % 2 == 1,
                         "Kernel column count must be odd.")
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    kernel.withUnsafeBufferPointer { k in
                        vDSP_imgfirD(src.baseAddress!,
                                     vDSP_Length(rowCount), vDSP_Length(columnCount),
                                     k.baseAddress!,
                                     dest.baseAddress!,
                                     vDSP_Length(kernelRowCount), vDSP_Length(kernelColumnCount))
                    }
                }
            }
    }
    
}
