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
    
    //===----------------------------------------------------------------------===//
    //
    //  Phase calculation
    //
    //===----------------------------------------------------------------------===//
    
    /// Populates `result` with the elementwise phase values, in radians, of the supplied complex vector; single-precision.
    ///
    /// - Parameter splitComplex: Single-precision complex input vector.
    /// - Parameter result: Single-precision real output vector.
    @inlinable
    public static func phase<V>(_ splitComplex: DSPSplitComplex,
                                result: inout V)
        where
        V: AccelerateMutableBuffer,
        V.Element == Float {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                withUnsafePointer(to: splitComplex) { src in
                    vDSP_zvphas(src, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Populates `result` with the elementwise phase values, in radians, of the supplied complex vector; double-precision.
    ///
    /// - Parameter splitComplex: Double-precision complex input vector.
    /// - Parameter result: Double-precision real output vector.
    @inlinable
    public static func phase<V>(_ splitComplex: DSPDoubleSplitComplex,
                                result: inout V)
        where
        V: AccelerateMutableBuffer,
        V.Element == Double {
            
            let n = vDSP_Length(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                withUnsafePointer(to: splitComplex) { src in
                    vDSP_zvphasD(src, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  Complex vector copying
    //
    //===----------------------------------------------------------------------===//
    
    /// Copies a complex vector; single-precision.
    ///
    /// - Parameter source: Single-precision complex input vector.
    /// - Parameter destination: Single-precision real output vector.
    @inlinable
    public static func copy(_ source: DSPSplitComplex,
                            to destination: inout DSPSplitComplex,
                            count: Int) {
        
        let n = vDSP_Length(count)
        
        withUnsafePointer(to: source) { src in
            vDSP_zvmov(src, 1,
                       &destination, 1,
                       n)
        }
    }
    
    /// Copies a complex vector; double-precision.
    ///
    /// - Parameter source: Double-precision complex input vector.
    /// - Parameter destination: Double-precision real output vector.
    @inlinable
    public static func copy(_ source: DSPDoubleSplitComplex,
                            to destination: inout DSPDoubleSplitComplex,
                            count: Int) {
        
        let n = vDSP_Length(count)
        
        withUnsafePointer(to: source) { src in
            vDSP_zvmovD(src, 1,
                        &destination, 1,
                        n)
        }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  Complex vector conjugate
    //
    //===----------------------------------------------------------------------===//
    
    /// Populates `result` with the conjugate of `splitComplex`; single-precision.
    ///
    /// - Parameter splitComplex: the `A` in `C[n] = conj(A[n])`.
    /// - Parameter result: The `C` in `C[n] = conj(A[n])`.
    @inlinable
    public static func conjugate(_ splitComplex: DSPSplitComplex,
                                 count: Int,
                                 result: inout DSPSplitComplex) {
        
        withUnsafePointer(to: splitComplex) { a in
            vDSP_zvconj(a, 1,
                        &result, 1,
                        vDSP_Length(count))
        }
    }
    
    /// Populates `result` with the conjugate of `splitComplex`; double-precision.
    ///
    /// - Parameter splitComplex: the `A` in `C[n] = conj(A[n])`.
    /// - Parameter result: The `C` in `C[n] = conj(A[n])`.
    @inlinable
    public static func conjugate(_ splitComplex: DSPDoubleSplitComplex,
                                 count: Int,
                                 result: inout DSPDoubleSplitComplex) {
        
        withUnsafePointer(to: splitComplex) { a in
            vDSP_zvconjD(a, 1,
                         &result, 1,
                         vDSP_Length(count))
        }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  Complex vector arithmetic
    //
    //===----------------------------------------------------------------------===//
    
    /// Populates `result` with the elementwise division of `splitComplex` and `vector`,
    /// single-precision.
    ///
    /// - Parameter splitComplex: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<U>(_ splitComplex: DSPSplitComplex,
                                 by vector: U,
                                 result: inout DSPSplitComplex)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            withUnsafePointer(to: splitComplex) { a in
                vector.withUnsafeBufferPointer { b in
                    vDSP_zrvdiv(a, 1,
                                b.baseAddress!, 1,
                                &result, 1,
                                vDSP_Length(vector.count))
                }
            }
    }
    
    /// Populates `result` with the elementwise division of `splitComplex` and `vector`,
    /// double-precision.
    ///
    /// - Parameter splitComplex: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<U>(_ splitComplex: DSPDoubleSplitComplex,
                                 by vector: U,
                                 result: inout DSPDoubleSplitComplex)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            withUnsafePointer(to: splitComplex) { a in
                vector.withUnsafeBufferPointer { b in
                    vDSP_zrvdivD(a, 1,
                                 b.baseAddress!, 1,
                                 &result, 1,
                                 vDSP_Length(vector.count))
                }
            }
    }
    
    /// Populates `result` with the elementwise product of `splitComplex` and `vector`,
    /// single-precision.
    ///
    /// - Parameter splitComplex: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<U>(_ splitComplex: DSPSplitComplex,
                                   by vector: U,
                                   result: inout DSPSplitComplex)
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            withUnsafePointer(to: splitComplex) { a in
                vector.withUnsafeBufferPointer { b in
                    vDSP_zrvmul(a, 1,
                                b.baseAddress!, 1,
                                &result, 1,
                                vDSP_Length(vector.count))
                }
            }
    }
    
    /// Populates `result` with the elementwise product of `splitComplex` and `vector`,
    /// double-precision.
    ///
    /// - Parameter splitComplex: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<U>(_ splitComplex: DSPDoubleSplitComplex,
                                   by vector: U,
                                   result: inout DSPDoubleSplitComplex)
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            withUnsafePointer(to: splitComplex) { a in
                vector.withUnsafeBufferPointer { b in
                    vDSP_zrvmulD(a, 1,
                                 b.baseAddress!, 1,
                                 &result, 1,
                                 vDSP_Length(vector.count))
                }
            }
    }
    
    /// Populates `result` with the elementwise product of `splitComplexA` and `splitComplexB`,
    /// optionally conjugating one of them; single-precision
    ///
    /// - Parameter splitComplexA: The `a` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    /// - Parameter splitComplexB: The `b` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter useConjugate: Specifies whether to multiply the complex conjugates of `splitComplexA`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    @inlinable
    public static func multiply(_ splitComplexA: DSPSplitComplex,
                                by splitComplexB: DSPSplitComplex,
                                count: Int,
                                useConjugate: Bool,
                                result: inout DSPSplitComplex) {
        
        let conjugate: Int32 = useConjugate ? -1 : 1
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvmul(a, 1,
                           b, 1,
                           &result, 1,
                           vDSP_Length(count),
                           conjugate)
            }
        }
    }
    
    /// Populates `result` with the elementwise product of `splitComplexA` and `splitComplexB`,
    /// optionally conjugating one of them; double-precision
    ///
    /// - Parameter splitComplexA: The `a` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    /// - Parameter splitComplexB: The `b` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter useConjugate: Specifies whether to multiply the complex conjugates of `splitComplexA`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]` or `c[i] = conj(a[i]) * b[i]`.
    @inlinable
    public static func multiply(_ splitComplexA: DSPDoubleSplitComplex,
                                by splitComplexB: DSPDoubleSplitComplex,
                                count: Int,
                                useConjugate: Bool,
                                result: inout DSPDoubleSplitComplex) {
        
        let conjugate: Int32 = useConjugate ? -1 : 1
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvmulD(a, 1,
                            b, 1,
                            &result, 1,
                            vDSP_Length(count),
                            conjugate)
            }
        }
    }
    
    /// Populates `result` with the elementwise sum of `splitComplexA` and `splitComplexB`,
    /// single-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add(_ splitComplexA: DSPSplitComplex,
                           to splitComplexB: DSPSplitComplex,
                           count: Int,
                           result: inout DSPSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvadd(a, 1,
                           b, 1,
                           &result, 1,
                           vDSP_Length(count))
            }
        }
    }
    
    /// Populates `result` with the elementwise sum of `splitComplexA` and `splitComplexB`,
    /// double-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add(_ splitComplexA: DSPDoubleSplitComplex,
                           to splitComplexB: DSPDoubleSplitComplex,
                           count: Int,
                           result: inout DSPDoubleSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvaddD(a, 1,
                            b, 1,
                            &result, 1,
                            vDSP_Length(count))
            }
        }
    }
    
    /// Populates `result` with the elementwise division of `splitComplexA` and `splitComplexB`,
    /// single-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide(_ splitComplexA: DSPSplitComplex,
                              by splitComplexB: DSPSplitComplex,
                              count: Int,
                              result: inout DSPSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvdiv(b, 1,
                           a, 1,
                           &result, 1,
                           vDSP_Length(count))
            }
        }
    }
    
    /// Populates `result` with the elementwise division of `splitComplexA` and `splitComplexB`,
    /// single-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide(_ splitComplexA: DSPDoubleSplitComplex,
                              by splitComplexB: DSPDoubleSplitComplex,
                              count: Int,
                              result: inout DSPDoubleSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvdivD(b, 1,
                            a, 1,
                            &result, 1,
                            vDSP_Length(count))
            }
        }
    }
    
    /// Populates `result` with the elementwise difference of `splitComplexA` and `splitComplexB`,
    /// single-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract(_ splitComplexB: DSPSplitComplex,
                                from splitComplexA: DSPSplitComplex,
                                count: Int,
                                result: inout DSPSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvsub(b, 1,
                           a, 1,
                           &result, 1,
                           vDSP_Length(count))
            }
        }
    }
    
    /// Populates `result` with the elementwise difference of `splitComplexA` and `splitComplexB`,
    /// double-precision.
    ///
    /// - Parameter splitComplexA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter splitComplexB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter count: The number of elements to process.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract(_ splitComplexB: DSPDoubleSplitComplex,
                                from splitComplexA: DSPDoubleSplitComplex,
                                count: Int,
                                result: inout DSPDoubleSplitComplex) {
        
        withUnsafePointer(to: splitComplexA) { a in
            withUnsafePointer(to: splitComplexB) { b in
                vDSP_zvsubD(b, 1,
                            a, 1,
                            &result, 1,
                            vDSP_Length(count))
            }
        }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  Complex vector absolute
    //
    //===----------------------------------------------------------------------===//
    
    /// Populates `result` with the absolute values of `vector`,
    /// single-precision.
    ///
    /// - Parameter vector: The input vector.
    /// - Parameter result: The output vector.
    @inlinable
    public static func absolute<V>(_ vector: DSPSplitComplex,
                                   result: inout V)
        where
        V: AccelerateMutableBuffer,
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
    @inlinable
    public static func absolute<V>(_ vector: DSPDoubleSplitComplex,
                                   result: inout V)
        where
        V: AccelerateMutableBuffer,
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
    
    //===----------------------------------------------------------------------===//
    //
    //  Complex squared magnitude
    //
    //===----------------------------------------------------------------------===//
    
    /// Calculates the squared magnitude of each element in `vector`, writing the result to `result`; single-precision.
    ///
    /// - Parameter splitComplex: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func squareMagnitudes<V>(_ splitComplex: DSPSplitComplex,
                                           result: inout V)
        where
        V : AccelerateMutableBuffer,
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
    
    @inlinable
    /// Calculates the squared magnitude of each element in `vector`, writing the result to `result`; double-precision.
    ///
    /// - Parameter splitComplex: Input values.
    /// - Parameter result: Output values.
    public static func squareMagnitudes<V>(_ splitComplex: DSPDoubleSplitComplex,
                                           result: inout V)
        where
        V : AccelerateMutableBuffer,
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
