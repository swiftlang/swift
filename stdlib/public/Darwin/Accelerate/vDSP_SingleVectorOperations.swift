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

import Accelerate

//===----------------------------------------------------------------------===//
//
//  vDSP Extrema
//
//===----------------------------------------------------------------------===//

extension vDSP {
    
    // MARK: Elementwise minimum
    
    /// Populates `result` with the lesser of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]
    /// - Parameter result: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func minimum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the lesser of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] < b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] < b[i] ? a[i] : b[i]
    /// - Parameter result: the `c` in `c[i] = a[i] < b[i] ? a[i] : b[i]
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func minimum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the greater of the corresponding values in `vectorA` and `vectorB`, single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]
    /// - Parameter result: the `c` in `c[i] = a[i] > b[i] ? a[i] : b[i]
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func maximum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the greater of the corresponding values in `vectorA` and `vectorB`, double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] > b[i] ? a[i] : b[i]`
    /// - Parameter vectorB: the `b` in `c[i] = a[i] > b[i] ? a[i] : b[i]
    /// - Parameter result: the `c` in `c[i] = a[i] > b[i] ? a[i] : b[i]
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func maximum<U, V>(_ vectorA: U,
                                     _ vectorB: U,
                                     result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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

extension vDSP {
    
    /// Populates `result` with the absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func absolute<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func absolute<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the negative absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func negativeAbsolute<U, V>(_ vector: U,
                                              result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the negative absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func negativeAbsolute<U, V>(_ vector: U,
                                              result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the negative values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func negative<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    /// Populates `result` with the negative values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func negative<U, V>(_ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    
    // MARK: Complex
    
    /// Populates `result` with the absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func absolute<V>(_ vector: DSPSplitComplex,
                                   result: inout V)
        where
        V: _MutableContiguousCollection,
        V.Element == Float {
            
            let n = result.count
            
            result.withUnsafeMutableBufferPointer { r in
                withUnsafePointer(to: vector) { v in
                    vDSP_zvabs(v, 1,
                               r.baseAddress!, 1,
                               vDSP_Length(n))
                }
            }
    }
    
    /// Populates `result` with the absolute values of `vector`,
    /// double-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func absolute<V>(_ vector: DSPDoubleSplitComplex,
                                   result: inout V)
        where
        V: _MutableContiguousCollection,
        V.Element == Double {
            
            let n = result.count
            
            result.withUnsafeMutableBufferPointer { r in
                withUnsafePointer(to: vector) { v in
                    vDSP_zvabsD(v, 1,
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

extension vDSP {
    
    // MARK: Reversing
    
    /// Reverses an array of single-precision values in-place.
    ///
    /// - Parameter vector: The array to reverse.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func reverse<V>(_ vector: inout V)
        where
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func reverse<V>(_ vector: inout V)
        where
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func sort<V>(_ vector: inout V,
                               sortOrder: SortOrder)
        where
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func sort<V>(_ vector: inout V,
                               sortOrder: SortOrder)
        where
        V: _MutableContiguousCollection,
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

extension vDSP {
    
    // MARK: Square
    
    /// Calculates the square of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func square<U, V>(_ vector: U,
                                    result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the square of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func square<U, V>(_ vector: U,
                                    result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the signed square of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func signedSquare<U, V>(_ vector: U,
                                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the signed square of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func signedSquare<U, V>(_ vector: U,
                                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Truncates to fraction each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Truncates to fraction each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func countZeroCrossings<U>(_ vector: U) -> UInt
        where
        U : _ContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func countZeroCrossings<U>(_ vector: U) -> UInt
        where
        U : _ContiguousCollection,
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
    
    // MARK: Signed Magnitudes
    
    /// Calculates the signed magnitude of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func squareMagnitudes<V>(_ splitComplex: DSPSplitComplex,
                                           result: inout V)
        where
        V : _MutableContiguousCollection,
        V.Element == Float {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                withUnsafePointer(to: splitComplex) { src in
                    vDSP_zvmags(src, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    /// Calculates the signed magnitude of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    public static func squareMagnitudes<V>(_ splitComplex: DSPDoubleSplitComplex,
                                           result: inout V)
        where
        V : _MutableContiguousCollection,
        V.Element == Double {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                withUnsafePointer(to: splitComplex) { src in
                    vDSP_zvmagsD(src, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
}
