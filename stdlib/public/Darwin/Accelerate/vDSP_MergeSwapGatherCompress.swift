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

// MARK: Merge and Swap

extension vDSP {
    
    // MARK: vDSP_vtmerg
    
    /// Tapered merge of two single-precision vectors.
    ///
    /// - Parameter vectorA: First input vector.
    /// - Parameter vectorB: Second input vector.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func taperedMerge<T, U, V>(_ vectorA: T,
                                             _ vectorB: U,
                                             result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(vectorA.count == vectorB.count,
                         "The number of elements in `vectorA` must equal the number of elements in `vectorB`.")
            precondition(vectorA.count == result.count,
                         "The number of elements in `vectorA` must equal the number of elements in `result`.")
            
            let n = vDSP_Length(vectorA.count)
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        
                        vDSP_vtmerg(a.baseAddress!, 1,
                                    b.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    /// Tapered merge of two double-precision vectors.
    ///
    /// - Parameter vectorA: First input vector.
    /// - Parameter vectorB: Second input vector.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func taperedMerge<T, U, V>(_ vectorA: T,
                                             _ vectorB: U,
                                             result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(vectorA.count == vectorB.count,
                         "The number of elements in `vectorA` must equal the number of elements in `vectorB`.")
            precondition(vectorA.count == result.count,
                         "The number of elements in `vectorA` must equal the number of elements in `result`.")
            
            let n = vDSP_Length(vectorA.count)
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        
                        vDSP_vtmergD(a.baseAddress!, 1,
                                     b.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Returns tapered merge of two single-precision vectors.
    ///
    /// - Parameter vectorA: First input vector.
    /// - Parameter vectorB: Second input vector.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func taperedMerge<T, U>(_ vectorA: T,
                                          _ vectorB: U) -> [Float]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let resultCount = vectorA.count
            
            let result = Array<Float>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.taperedMerge(vectorA,
                                  vectorB,
                                  result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    /// Returns tapered merge of two double-precision vectors.
    ///
    /// - Parameter vectorA: First input vector.
    /// - Parameter vectorB: Second input vector.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func taperedMerge<T, U>(_ vectorA: T,
                                          _ vectorB: U) -> [Double]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let resultCount = vectorA.count
            
            let result = Array<Double>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.taperedMerge(vectorA,
                                  vectorB,
                                  result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    // MARK: vDSP_vswap
    
    /// Swaps the elements of two single-precision vectors.
    ///
    /// - Parameter vectorA: First input-output vector.
    /// - Parameter vectorB: Second input-output vector.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func swapElements<T, U>(_ vectorA: inout T,
                                          _ vectorB: inout U)
        where
        T: AccelerateMutableBuffer, U: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float {
            
            precondition(vectorA.count == vectorB.count,
                         "The number of elements in `vectorA` must equal the number of elements in `vectorB`.")
            
            let n = vDSP_Length(vectorA.count)
            
            vectorA.withUnsafeMutableBufferPointer { a in
                vectorB.withUnsafeMutableBufferPointer { b in
                    vDSP_vswap(a.baseAddress!, 1,
                               b.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Swaps the elements of two double-precision vectors.
    ///
    /// - Parameter vectorA: First input-output vector.
    /// - Parameter vectorB: Second input-output vector.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func swapElements<T, U>(_ vectorA: inout T,
                                          _ vectorB: inout U)
        where
        T: AccelerateMutableBuffer, U: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double {
            
            precondition(vectorA.count == vectorB.count,
                         "The number of elements in `vectorA` must equal the number of elements in `vectorB`.")
            
            let n = vDSP_Length(vectorA.count)
            
            vectorA.withUnsafeMutableBufferPointer { a in
                vectorB.withUnsafeMutableBufferPointer { b in
                    vDSP_vswapD(a.baseAddress!, 1,
                                b.baseAddress!, 1,
                                n)
                }
            }
    }
}

// MARK: Gather and Compress

extension vDSP {
    
    // MARK: vDSP_vgathr
    
    /// Gathers single-precision vector based on values in an indices vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter indices: One-based indices to copy selected elements of source vector to sequential locations in result.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func gather<T, U, V>(_ vector: T,
                                       indices: U,
                                       result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == UInt, V.Element == Float {
            precondition(indices.count == result.count,
                         "The number of elements in `indices` must equal the number of elements in `result`.")
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    indices.withUnsafeBufferPointer { i in
                        vDSP_vgathr(v.baseAddress!,
                                    i.baseAddress!, 1,
                                    r.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    /// Gathers double-precision vector based on values in an indices vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter indices: One-based indices to copy selected elements of source vector to sequential locations in result.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func gather<T, U, V>(_ vector: T,
                                       indices: U,
                                       result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == UInt, V.Element == Double {
            precondition(indices.count == result.count,
                         "The number of elements in `indices` must equal the number of elements in `result`.")
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    indices.withUnsafeBufferPointer { i in
                        vDSP_vgathrD(v.baseAddress!,
                                     i.baseAddress!, 1,
                                     r.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Returns gathered single-precision vector based on values in an indices vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter indices: One-based indices to copy selected elements of source vector to sequential locations in result.
    ///
    /// - Returns: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func gather<T, U>(_ vector: T,
                                    indices: U) -> [Float]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Float, U.Element == UInt {
            
            let resultCount = indices.count
            
            let result = Array<Float>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.gather(vector,
                            indices: indices,
                            result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    /// Returns gathered double-precision vector based on values in an indices vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter indices: One-based indices to copy selected elements of source vector to sequential locations in result.
    ///
    /// - Returns: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func gather<T, U>(_ vector: T,
                                    indices: U) -> [Double]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Double, U.Element == UInt {
            
            let resultCount = indices.count
            
            let result = Array<Double>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.gather(vector,
                            indices: indices,
                            result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
    // MARK: vDSP_vcmprs
    
    /// Compresses single-precision vector based on nonzero values in a gating vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter gatingVector: Gating vector; for nonzero elements, corresponding elements of source vector are sequentially copied to output result.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func compress<T, U, V>(_ vector: T,
                                         gatingVector: U,
                                         result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(vector.count == gatingVector.count,
                         "The number of elements in `gatingVector` must equal the number of elements in `vector`.")
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    gatingVector.withUnsafeBufferPointer { g in
                        vDSP_vcmprs(v.baseAddress!, 1,
                                    g.baseAddress!, 1,
                                    r.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    /// Compresses double-precision vector based on nonzero values in a gating vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter gatingVector: Gating vector; for nonzero elements, corresponding elements of source vector are sequentially copied to output result.
    /// - Parameter result: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func compress<T, U, V>(_ vector: T,
                                         gatingVector: U,
                                         result: inout V)
        where
        T: AccelerateBuffer, U: AccelerateBuffer, V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(vector.count == gatingVector.count,
                         "The number of elements in `gatingVector` must equal the number of elements in `vector`.")
            
            let n = vDSP_Length(vector.count)
            
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    gatingVector.withUnsafeBufferPointer { g in
                        vDSP_vcmprsD(v.baseAddress!, 1,
                                     g.baseAddress!, 1,
                                     r.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Compresses single-precision vector based on nonzero values in a gating vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter gatingVector: Gating vector; for nonzero elements, corresponding elements of source vector are sequentially copied to output result.
    /// - Parameter nonZeroGatingCount: The number of nonzero elements in `gatingVector`; set to `nil` to have vDSP calculate this value for you.
    ///
    /// - Returns: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func compress<T, U>(_ vector: T,
                                      gatingVector: U,
                                      nonZeroGatingCount: Int?) -> [Float]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let resultCount: Int
            
            if let nonZeroGatingCount = nonZeroGatingCount {
                resultCount = nonZeroGatingCount
            } else {
                // Count elements in `gatingVector` that are non-zero.
                var lowCount: vDSP_Length = 0
                var highCount: vDSP_Length = 0
                var clippingThreshold: Float = 0
                
                _ = Array<Float>(unsafeUninitializedCapacity: gatingVector.count) {
                    buffer, initializedCount in
                    
                    gatingVector.withUnsafeBufferPointer { g in
                        vDSP_vclipc(g.baseAddress!, 1,
                                    &clippingThreshold, &clippingThreshold,
                                    buffer.baseAddress!, 1,
                                    vDSP_Length(gatingVector.count),
                                    &lowCount, &highCount)
                    }
                }
                
                resultCount = Int(lowCount + highCount)
            }
            
            let result = Array<Float>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.compress(vector,
                              gatingVector: gatingVector,
                              result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }

    /// Compresses double-precision vector based on nonzero values in a gating vector.
    ///
    /// - Parameter vector: Source vector:
    /// - Parameter gatingVector: Gating vector; for nonzero elements, corresponding elements of source vector are sequentially copied to output result.
    /// - Parameter nonZeroGatingCount: The number of nonzero elements in `gatingVector`; set to `nil` to have vDSP calculate this value for you.
    ///
    /// - Returns: Output values.
    @inlinable
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public static func compress<T, U>(_ vector: T,
                                      gatingVector: U,
                                      nonZeroGatingCount: Int?) -> [Double]
        where
        T: AccelerateBuffer, U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let resultCount: Int
            
            if let nonZeroGatingCount = nonZeroGatingCount {
                resultCount = nonZeroGatingCount
            } else {
                // Count elements in `gatingVector` that are non-zero.
                var lowCount: vDSP_Length = 0
                var highCount: vDSP_Length = 0
                var clippingThreshold: Double = 0
                
                _ = Array<Double>(unsafeUninitializedCapacity: gatingVector.count) {
                    buffer, initializedCount in
                    
                    gatingVector.withUnsafeBufferPointer { g in
                        vDSP_vclipcD(g.baseAddress!, 1,
                                     &clippingThreshold, &clippingThreshold,
                                     buffer.baseAddress!, 1,
                                     vDSP_Length(gatingVector.count),
                                     &lowCount, &highCount)
                    }
                }
                
                resultCount = Int(lowCount + highCount)
            }
            
            let result = Array<Double>(unsafeUninitializedCapacity: resultCount) {
                buffer, initializedCount in
                
                vDSP.compress(vector,
                              gatingVector: gatingVector,
                              result: &buffer)
                
                initializedCount = resultCount
            }
            
            return result
    }
    
}
