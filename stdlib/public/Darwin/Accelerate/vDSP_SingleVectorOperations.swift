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

//===----------------------------------------------------------------------===//
//
//  vDSP Extrema
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Elementwise minimum
    
    /// Returns an array containing the lesser of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func minimum<U>(_ vectorA: U,
                                  _ vectorB: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            precondition(vectorA.count == vectorB.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                minimum(vectorA,
                        vectorB,
                        result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the lesser of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter result: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func minimum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = vDSP_Length(min(vectorA.count,
                                    vectorB.count,
                                    result.count))
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vmin(a.baseAddress!, 1,
                                  b.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  n)
                    }
                }
            }
    }
    
    /// Returns an array containing the lesser of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func minimum<U>(_ vectorA: U,
                                  _ vectorB: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            precondition(vectorA.count == vectorB.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                minimum(vectorA,
                        vectorB,
                        result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the lesser of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter result: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func minimum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = vDSP_Length(min(vectorA.count,
                                    vectorB.count,
                                    result.count))
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vminD(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
    
    // MARK: Elementwise maximum
    
    /// Returns an array containing the greater of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    @inlinable
    public static func maximum<U>(_ vectorA: U,
                                  _ vectorB: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            precondition(vectorA.count == vectorB.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                maximum(vectorA,
                        vectorB,
                        result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the greater of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter result: the `c` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    @inlinable
    public static func maximum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = vDSP_Length(min(vectorA.count,
                                    vectorB.count,
                                    result.count))
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vmax(a.baseAddress!, 1,
                                  b.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  n)
                    }
                }
            }
    }
    
    /// Returns an array containing the greater of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = `c[i] = a[i] > b[i] ? a[i] : b[i]`
    @inlinable
    public static func maximum<U>(_ vectorA: U,
                                  _ vectorB: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            precondition(vectorA.count == vectorB.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                maximum(vectorA,
                        vectorB,
                        result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the greater of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter result: the `c` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    @inlinable
    public static func maximum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = vDSP_Length(min(vectorA.count,
                                    vectorB.count,
                                    result.count))
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vmaxD(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Absolute and Negation
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    /// Returns an array containing the absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func absolute<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                absolute(vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func absolute<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vabs(v.baseAddress!, 1,
                              r.baseAddress!, 1,
                              vDSP_Length(n))
                }
            }
            
    }
    
    /// Returns an array containing the absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func absolute<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                absolute(vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func absolute<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vabsD(v.baseAddress!, 1,
                               r.baseAddress!, 1,
                               vDSP_Length(n))
                }
            }
    }
    
    /// Returns an array containing the negative absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negativeAbsolute<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                negativeAbsolute(vector,
                                 result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the negative absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negativeAbsolute<U, V>(_ vector: U,
                                              result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vnabs(v.baseAddress!, 1,
                               r.baseAddress!, 1,
                               vDSP_Length(n))
                }
            }
    }
    
    /// Returns an array containing the negative absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negativeAbsolute<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                negativeAbsolute(vector,
                                 result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the negative absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negativeAbsolute<U, V>(_ vector: U,
                                              result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vnabsD(v.baseAddress!, 1,
                                r.baseAddress!, 1,
                                vDSP_Length(n))
                }
            }
    }
    
    /// Returns an array containing the negative values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negative<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                negative(vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the negative values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negative<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vneg(v.baseAddress!, 1,
                              r.baseAddress!, 1,
                              vDSP_Length(n))
                }
            }
    }
    
    /// Returns an array containing the negative values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negative<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                negative(vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the negative values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func negative<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    vDSP_vnegD(v.baseAddress!, 1,
                               r.baseAddress!, 1,
                               vDSP_Length(n))
                }
            }
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP In-place reversing and sorting
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Reversing
    
    /// Reverses an array of single-precision values in-place.
    ///
    /// - Parameter vector: The array to reverse.
    @inlinable
    public static func reverse<V>(_ vector: inout V)
        where
        V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vrvrs(v.baseAddress!, 1,
                           n)
            }
    }
    
    /// Reverses an array of double-precision values in-place.
    ///
    /// - Parameter vector: The array to reverse.
    @inlinable
    public static func reverse<V>(_ vector: inout V)
        where
        V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vrvrsD(v.baseAddress!, 1,
                            n)
            }
    }
    
    // MARK: Sorting
    public enum SortOrder: Int32 {
        case ascending = 1
        case descending = -1
    }
    
    /// Sorts an array of single-precision values in-place.
    ///
    /// - Parameter vector: The array to sort.
    /// - Parameter sortOrder: The sort direction.
    @inlinable
    public static func sort<V>(_ vector: inout V,
                               sortOrder: SortOrder)
        where
        V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vsort(v.baseAddress!,
                           n,
                           sortOrder.rawValue)
            }
    }
    
    /// Sorts an array of double-precision values in-place.
    ///
    /// - Parameter vector: The array to sort.
    /// - Parameter sortOrder: The sort direction.
    @inlinable
    public static func sort<V>(_ vector: inout V,
                               sortOrder: SortOrder)
        where
        V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(vector.count)
            
            vector.withUnsafeMutableBufferPointer { v in
                vDSP_vsortD(v.baseAddress!,
                            n,
                            sortOrder.rawValue)
            }
    }
}

//===----------------------------------------------------------------------===//
//
//  vDSP Single vector arithmetic
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Square
    
    /// Returns an array containing the square of each element in `vector`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func square<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                square(vector,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the square of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func square<U, V>(_ vector: U,
                                    result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vsq(src.baseAddress!, 1,
                             dest.baseAddress!, 1,
                             n)
                }
            }
    }
    
    /// Returns an array containing the square of each element in `vector`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func square<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                square(vector,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the square of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func square<U, V>(_ vector: U,
                                    result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vsqD(src.baseAddress!, 1,
                              dest.baseAddress!, 1,
                              n)
                }
            }
    }
    
    // MARK: Signed Square
    
    /// Returns an array containing the signed square of each element in `vector`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func signedSquare<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                signedSquare(vector,
                             result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the signed square of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func signedSquare<U, V>(_ vector: U,
                                          result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vssq(src.baseAddress!, 1,
                              dest.baseAddress!, 1,
                              n)
                }
            }
    }
    
    /// Returns an array containing the signed square of each element in `vector`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func signedSquare<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                signedSquare(vector,
                             result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the signed square of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func signedSquare<U, V>(_ vector: U,
                                          result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vssqD(src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
    
    // MARK: Truncate to Fraction
    
    /// Returns an array containing each element in `vector` truncated to fraction, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func trunc<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                trunc(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Truncates to fraction each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vfrac(src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Returns an array containing each element in `vector` truncated to fraction, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Returns: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    @inlinable
    public static func trunc<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                trunc(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Truncates to fraction each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vDSP_vfracD(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    // Zero crossing
    
    /// Returns the number of zero crossings in `vector`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: The total number of transitions from positive to negative values and from negative to positive values.
    @inlinable
    public static func countZeroCrossings<U>(_ vector: U) -> UInt
        where
        U : AccelerateBuffer,
        U.Element == Float {
            
            let n = vDSP_Length(vector.count)
            var crossingCount: vDSP_Length = 0
            var lastCrossingIndex: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { src in
                vDSP_nzcros(src.baseAddress!, 1,
                            n,
                            &lastCrossingIndex,
                            &crossingCount,
                            n)
            }
            
            return crossingCount
    }
    
    /// Returns the number of zero crossings in `vector`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: The total number of transitions from positive to negative values and from negative to positive values.
    @inlinable
    public static func countZeroCrossings<U>(_ vector: U) -> UInt
        where
        U : AccelerateBuffer,
        U.Element == Double {
            
            let n = vDSP_Length(vector.count)
            var crossingCount: vDSP_Length = 0
            var lastCrossingIndex: vDSP_Length = 0
            
            vector.withUnsafeBufferPointer { src in
                vDSP_nzcrosD(src.baseAddress!, 1,
                             n,
                             &lastCrossingIndex,
                             &crossingCount,
                             n)
            }
            
            return crossingCount
    }
}
