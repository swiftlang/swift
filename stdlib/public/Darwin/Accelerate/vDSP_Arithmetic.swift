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

// Vector-vector and vector-scalar arithmetic

extension vDSP {
    
    // MARK: c[i] = a[i] + b                                vDSP_vsadd
    
    /// Populates `result` with the elementwise sum of `vector` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(_ scalar: Float,
                                 _ vector: U,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsadd(v.baseAddress!, 1,
                                   s,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of `vector` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(_ scalar: Double,
                                 _ vector: U,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsaddD(v.baseAddress!, 1,
                                    s,
                                    r.baseAddress!, 1,
                                    vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] + b[i]                             vDSP_vadd
    
    /// Populates `result` with the elementwise sum of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(_ vectorA: T,
                                    _ vectorB: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vadd(a.baseAddress!, 1,
                                  b.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(_ vectorA: T,
                                    _ vectorB: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vaddD(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] - b[i]                             vDSP_vsub
    
    /// Populates `result` with the elementwise difference of `vector` and `fromVector`,
    /// single-precision.
    ///
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vectorB: T,
                                         from vectorA: U,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorB.withUnsafeBufferPointer { a in
                    vectorA.withUnsafeBufferPointer { b in
                        vDSP_vsub(a.baseAddress!, 1,
                                  b.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise difference of `vector` and `fromVector`,
    /// double-precision.
    ///
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vectorB: T,
                                         from vectorA: U,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorB.withUnsafeBufferPointer { a in
                    vectorA.withUnsafeBufferPointer { b in
                        vDSP_vsubD(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] * b                                vDSP_vsmul
    
    /// Populates `result` with the elementwise product of `vector` and `scalar
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<U, V>(_ scalar: Float,
                                      _ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsmul(v.baseAddress!, 1,
                                   s,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of `vector` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<U, V>(_ scalar: Double,
                                      _ vector: U,
                                      result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsmulD(v.baseAddress!, 1,
                                    s,
                                    r.baseAddress!, 1,
                                    vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] * b[i]                                 vDSP_vmul
    
    /// Populates `result` with the elementwise product of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(_ vectorA: T,
                                         _ vectorB: U,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vmul(a.baseAddress!, 1,
                                  b.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(_ vectorA: T,
                                         _ vectorB: U,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vmulD(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] / b                                vDSP_vsdiv
    
    /// Populates `result` with the elementwise division of `vector` by `scalar`,
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b`
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<U, V>(_ vector: U,
                                    by scalar: Float,
                                    result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsdiv(v.baseAddress!, 1,
                                   [scalar],
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise division of `vector` by `scalar`,
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b`
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<U, V>(_ vector: U,
                                    by scalar: Double,
                                    result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_vsdivD(v.baseAddress!, 1,
                                    s,
                                    r.baseAddress!, 1,
                                    vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a / b[i]                                vDSP_svdiv
    
    /// Populates `result` with the elementwise division of `scalar` by `vector`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a / b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<U, V>(_ scalar: Float,
                                    by vector: U,
                                    result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_svdiv(s,
                                   v.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise division of `scalar` by `vector`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a / b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<U, V>(_ scalar: Double,
                                    by vector: U,
                                    result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vector.withUnsafeBufferPointer { v in
                    withUnsafePointer(to: scalar) { s in
                        vDSP_svdivD(s,
                                    v.baseAddress!, 1,
                                    r.baseAddress!, 1,
                                    vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: c[i] = a[i] / b[i]                             vDSP_vdiv
    
    /// Populates `result` with the elementwise division of `vectorA` by `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<T, U, V>(_ vectorA: T,
                                       by vectorB: U,
                                       result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vdiv(b.baseAddress!, 1,
                                  a.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise division of `vectorA` by `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func divide<T, U, V>(_ vectorA: T,
                                       by vectorB: U,
                                       result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            result.withUnsafeMutableBufferPointer { r in
                vectorA.withUnsafeBufferPointer { a in
                    vectorB.withUnsafeBufferPointer { b in
                        vDSP_vdivD(b.baseAddress!, 1,
                                   a.baseAddress!, 1,
                                   r.baseAddress!, 1,
                                   vDSP_Length(n))
                    }
                }
            }
    }
    
    // MARK: o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]  vDSP_vaddsub
    
    /// Calculates elementwise sum and difference of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `i0` in `o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter vectorB: the `i1` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter addResult: The `o0` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter subtractResult: The `o1` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func addSubtract<S, T, U, V>(_ vectorA: S,
                                               _ vectorB: T,
                                               addResult: inout U,
                                               subtractResult: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _MutableContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = addResult.count
            precondition(vectorA.count == n &&
                vectorB.count == n &&
                subtractResult.count == n)
            
            addResult.withUnsafeMutableBufferPointer { o0 in
                subtractResult.withUnsafeMutableBufferPointer { o1 in
                    vectorA.withUnsafeBufferPointer { i0 in
                        vectorB.withUnsafeBufferPointer { i1 in
                            vDSP_vaddsub(i0.baseAddress!, 1,
                                         i1.baseAddress!, 1,
                                         o0.baseAddress!, 1,
                                         o1.baseAddress!, 1,
                                         vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Calculates elementwise sum and difference of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `i0` in `o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter vectorB: the `i1` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter addResult: The `o0` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter subtractResult: The `o1` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func addSubtract<S, T, U, V>(_ vectorA: S,
                                               _ vectorB: T,
                                               addResult: inout U,
                                               subtractResult: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _MutableContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = addResult.count
            precondition(vectorA.count == n &&
                vectorB.count == n &&
                subtractResult.count == n)
            
            addResult.withUnsafeMutableBufferPointer { o0 in
                subtractResult.withUnsafeMutableBufferPointer { o1 in
                    vectorA.withUnsafeBufferPointer { i0 in
                        vectorB.withUnsafeBufferPointer { i1 in
                            vDSP_vaddsubD(i0.baseAddress!, 1,
                                          i1.baseAddress!, 1,
                                          o0.baseAddress!, 1,
                                          o1.baseAddress!, 1,
                                          vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] + b[i]) * c                       vDSP_vasm
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `vectorAddition` and `byScalar`,
    /// single-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(vectorAddition: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vasm(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      s,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `vectorAddition` and `byScalar`,
    /// double-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(vectorAddition: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vasmD(a.baseAddress!, 1,
                                       b.baseAddress!, 1,
                                       s,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] + b[i]) * c[i]                    vDSP_vam
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `vectorAddition` and `vector`,
    /// single-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorAddition: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vam(a.baseAddress!, 1,
                                     b.baseAddress!, 1,
                                     c.baseAddress!, 1,
                                     r.baseAddress!, 1,
                                     vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `vectorAddition` and `vector`,
    /// double-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorAddition: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vamD(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      c.baseAddress!, 1,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] - b[i]) * c                       vDSP_vsbsm
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `vectorSubtraction` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter vectorSubtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(vectorSubtraction: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorSubtraction.a.count == n &&
                vectorSubtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtraction.a.withUnsafeBufferPointer { a in
                    vectorSubtraction.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vsbsm(a.baseAddress!, 1,
                                       b.baseAddress!, 1,
                                       s,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `vectorSubtraction` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter vectorSubtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(vectorSubtraction: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorSubtraction.a.count == n &&
                vectorSubtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtraction.a.withUnsafeBufferPointer { a in
                    vectorSubtraction.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vsbsmD(a.baseAddress!, 1,
                                        b.baseAddress!, 1,
                                        s,
                                        r.baseAddress!, 1,
                                        vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] - b[i]) * c[i]                    vDSP_vsbm
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `vectorSubtraction` and `vector`,
    /// single-precision.
    ///
    /// - Parameter vectorSubtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorSubtraction: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorSubtraction.a.count == n &&
                vectorSubtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtraction.a.withUnsafeBufferPointer { a in
                    vectorSubtraction.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vsbm(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      c.baseAddress!, 1,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `vectorSubtraction` and `vector`,
    /// double-precision.
    ///
    /// - Parameter vectorSubtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorSubtraction: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorSubtraction.a.count == n &&
                vectorSubtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtraction.a.withUnsafeBufferPointer { a in
                    vectorSubtraction.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vsbmD(a.baseAddress!, 1,
                                       b.baseAddress!, 1,
                                       c.baseAddress!, 1,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = a[i]*b[i] + c;                       vDSP_vmsa
    
    /// Populates `result` with the elementwise sum of `scalar`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// single-precision.
    ///
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorMultiplication: (a: T, b: U),
                                    _ scalar: Float,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vmsa(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      s,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of `scalar`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// double-precision.
    ///
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorMultiplication: (a: T, b: U),
                                    _ scalar: Double,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        withUnsafePointer(to: scalar) { s in
                            vDSP_vmsaD(a.baseAddress!, 1,
                                       b.baseAddress!, 1,
                                       s,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] * b) + c[i]                       vDSP_vsma
    
    /// Populates `result` with the elementwise sum of `toVector`
    /// and the product of the vector and scalar in `vectorScalarMultiplication`,
    /// single-precision.
    ///
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorScalarMultiplication: (a: T, b: Float),
                                    _ vector: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    vector.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                            vDSP_vsma(a.baseAddress!, 1,
                                      b,
                                      c.baseAddress!, 1,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of `toVector`
    /// and the product of the vector and scalar in `vectorScalarMultiplication`,
    /// double-precision.
    ///
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorScalarMultiplication: (a: T, b: Double),
                                    _ vector: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    vector.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                            vDSP_vsmaD(a.baseAddress!, 1,
                                       b,
                                       c.baseAddress!, 1,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] * b[i]) + c[i]                    vDSP_vma
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// single-precision.
    ///
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<S, T, U, V>(vectorMultiplication: (a: S, b: T),
                                       _ vector: U,
                                       result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vma(a.baseAddress!, 1,
                                     b.baseAddress!, 1,
                                     c.baseAddress!, 1,
                                     r.baseAddress!, 1,
                                     vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// double-precision.
    ///
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<S, T, U, V>(vectorMultiplication: (a: S, b: T),
                                       _ vector: U,
                                       result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vmaD(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      c.baseAddress!, 1,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: d[i] = (a[i] * b[i]) - c[i]             vDSP_vmsb
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// single-precision.
    ///
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<S, T, U, V>(_ vector: S,
                                            fromVectorMultiplication vectorMultiplication: (a: T, b: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vmsb(a.baseAddress!, 1,
                                      b.baseAddress!, 1,
                                      c.baseAddress!, 1,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the two vectors in `vectorMultiplication`,
    /// double-precision.
    ///
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vectorMultiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<S, T, U, V>(_ vector: S,
                                            fromVectorMultiplication vectorMultiplication: (a: T, b: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorMultiplication.a.count == n &&
                vectorMultiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplication.a.withUnsafeBufferPointer { a in
                    vectorMultiplication.b.withUnsafeBufferPointer { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vmsbD(a.baseAddress!, 1,
                                       b.baseAddress!, 1,
                                       c.baseAddress!, 1,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] * b) + (c[i] * d)          vDSP_vsmsma
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-scalar products, single-precision.
    ///
    /// - Parameter vectorScalarMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter vectorScalarMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorScalarMultiplication vectorScalarMultiplicationAB: (a: T, b: Float),
                                    vectorScalarMultiplication vectorScalarMultiplicationCD: (c: U, d: Float),
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorScalarMultiplicationAB.a.count == n &&
                vectorScalarMultiplicationCD.c.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorScalarMultiplicationCD.c.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: vectorScalarMultiplicationAB.b) { b in
                            withUnsafePointer(to: vectorScalarMultiplicationCD.d) { d in
                                vDSP_vsmsma(a.baseAddress!, 1,
                                            b,
                                            c.baseAddress!, 1,
                                            d,
                                            r.baseAddress!, 1,
                                            vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-scalar products, double-precision.
    ///
    /// - Parameter vectorScalarMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter vectorScalarMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(vectorScalarMultiplication vectorScalarMultiplicationAB: (a: T, b: Double),
                                    vectorScalarMultiplication vectorScalarMultiplicationCD: (c: U, d: Double),
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorScalarMultiplicationAB.a.count == n &&
                vectorScalarMultiplicationCD.c.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorScalarMultiplicationCD.c.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: vectorScalarMultiplicationAB.b) { b in
                            withUnsafePointer(to: vectorScalarMultiplicationCD.d) { d in
                                vDSP_vsmsmaD(a.baseAddress!, 1,
                                             b,
                                             c.baseAddress!, 1,
                                             d,
                                             r.baseAddress!, 1,
                                             vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] * b[i]) + (c[i] * d[i])          vDSP_vmma
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter vectorMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter vectorMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<R, S, T, U, V>(vectorMultiplication vectorMultiplicationAB: (a: R, b: S),
                                          vectorMultiplication vectorMultiplicationCD: (c: T, d: U),
                                          result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorMultiplicationAB.a.count == n &&
                vectorMultiplicationAB.b.count == n &&
                vectorMultiplicationCD.c.count == n &&
                vectorMultiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorMultiplicationAB.b.withUnsafeBufferPointer { b in
                        vectorMultiplicationCD.c.withUnsafeBufferPointer { c in
                            vectorMultiplicationCD.d.withUnsafeBufferPointer { d in
                                vDSP_vmma(a.baseAddress!, 1,
                                          b.baseAddress!, 1,
                                          c.baseAddress!, 1,
                                          d.baseAddress!, 1,
                                          r.baseAddress!, 1,
                                          vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-vector products, double-precision.
    ///
    /// - Parameter vectorMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter vectorMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<R, S, T, U, V>(vectorMultiplication vectorMultiplicationAB: (a: R, b: S),
                                          vectorMultiplication vectorMultiplicationCD: (c: T, d: U),
                                          result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorMultiplicationAB.a.count == n &&
                vectorMultiplicationAB.b.count == n &&
                vectorMultiplicationCD.c.count == n &&
                vectorMultiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorMultiplicationAB.b.withUnsafeBufferPointer { b in
                        vectorMultiplicationCD.c.withUnsafeBufferPointer { c in
                            vectorMultiplicationCD.d.withUnsafeBufferPointer { d in
                                vDSP_vmmaD(a.baseAddress!, 1,
                                           b.baseAddress!, 1,
                                           c.baseAddress!, 1,
                                           d.baseAddress!, 1,
                                           r.baseAddress!, 1,
                                           vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] + b[i]) * (c[i] + d[i])    vDSP_vaam
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector sums, single-precision.
    ///
    /// - Parameter vectorAdditionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter vectorAdditionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorAddition vectorAdditionAB: (a: S, b: T),
                                            toVectorAddition vectorAdditionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorAdditionAB.a.count == n &&
                vectorAdditionAB.b.count == n &&
                vectorAdditionCD.c.count == n &&
                vectorAdditionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAdditionAB.a.withUnsafeBufferPointer { a in
                    vectorAdditionAB.b.withUnsafeBufferPointer { b in
                        vectorAdditionCD.c.withUnsafeBufferPointer { c in
                            vectorAdditionCD.d.withUnsafeBufferPointer { d in
                                vDSP_vaam(a.baseAddress!, 1,
                                          b.baseAddress!, 1,
                                          c.baseAddress!, 1,
                                          d.baseAddress!, 1,
                                          r.baseAddress!, 1,
                                          vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector sums, double-precision.
    ///
    /// - Parameter vectorAdditionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter vectorAdditionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(vectorAddition vectorAdditionAB: (a: S, b: T),
                                            toVectorAddition vectorAdditionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorAdditionAB.a.count == n &&
                vectorAdditionAB.b.count == n &&
                vectorAdditionCD.c.count == n &&
                vectorAdditionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAdditionAB.a.withUnsafeBufferPointer { a in
                    vectorAdditionAB.b.withUnsafeBufferPointer { b in
                        vectorAdditionCD.c.withUnsafeBufferPointer { c in
                            vectorAdditionCD.d.withUnsafeBufferPointer { d in
                                vDSP_vaamD(a.baseAddress!, 1,
                                           b.baseAddress!, 1,
                                           c.baseAddress!, 1,
                                           d.baseAddress!, 1,
                                           r.baseAddress!, 1,
                                           vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] * b[i]) - (c[i] * d[i])    vDSP_vmmsb
    
    /// Populates `result` with the elementwise difference of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter vectorMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter vectorMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<R, S, T, U, V>(vectorMultiplication vectorMultiplicationCD: (c: R, d: S),
                                               fromVectorMultiplication vectorMultiplicationAB: (a: T, b: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorMultiplicationAB.a.count == n &&
                vectorMultiplicationAB.b.count == n &&
                vectorMultiplicationCD.c.count == n &&
                vectorMultiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorMultiplicationAB.b.withUnsafeBufferPointer { b in
                        vectorMultiplicationCD.c.withUnsafeBufferPointer { c in
                            vectorMultiplicationCD.d.withUnsafeBufferPointer { d in
                                vDSP_vmmsb(a.baseAddress!, 1,
                                           b.baseAddress!, 1,
                                           c.baseAddress!, 1,
                                           d.baseAddress!, 1,
                                           r.baseAddress!, 1,
                                           vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise difference of two elementwise
    /// vector-vector products, double-precision.
    ///
    /// - Parameter vectorMultiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter vectorMultiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<R, S, T, U, V>(vectorMultiplication vectorMultiplicationCD: (c: R, d: S),
                                               fromVectorMultiplication vectorMultiplicationAB: (a: T, b: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorMultiplicationAB.a.count == n &&
                vectorMultiplicationAB.b.count == n &&
                vectorMultiplicationCD.c.count == n &&
                vectorMultiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorMultiplicationAB.a.withUnsafeBufferPointer { a in
                    vectorMultiplicationAB.b.withUnsafeBufferPointer { b in
                        vectorMultiplicationCD.c.withUnsafeBufferPointer { c in
                            vectorMultiplicationCD.d.withUnsafeBufferPointer { d in
                                vDSP_vmmsbD(a.baseAddress!, 1,
                                            b.baseAddress!, 1,
                                            c.baseAddress!, 1,
                                            d.baseAddress!, 1,
                                            r.baseAddress!, 1,
                                            vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] - b[i]) * (c[i] - d[i])    vDSP_vsbsbm
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector differences, single-precision.
    ///
    /// - Parameter vectorSubtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter vectorSubtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(vectorSubtraction vectorSubtractionAB: (a: R, b: S),
                                               vectorSubtraction vectorSubtractionCD: (c: T, d: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorSubtractionAB.a.count == n &&
                vectorSubtractionAB.b.count == n &&
                vectorSubtractionCD.c.count == n &&
                vectorSubtractionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtractionAB.a.withUnsafeBufferPointer { a in
                    vectorSubtractionAB.b.withUnsafeBufferPointer { b in
                        vectorSubtractionCD.c.withUnsafeBufferPointer { c in
                            vectorSubtractionCD.d.withUnsafeBufferPointer { d in
                                vDSP_vsbsbm(a.baseAddress!, 1,
                                            b.baseAddress!, 1,
                                            c.baseAddress!, 1,
                                            d.baseAddress!, 1,
                                            r.baseAddress!, 1,
                                            vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector differences, double-precision.
    ///
    /// - Parameter vectorSubtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter vectorSubtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(vectorSubtraction vectorSubtractionAB: (a: R, b: S),
                                               vectorSubtraction vectorSubtractionCD: (c: T, d: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorSubtractionAB.a.count == n &&
                vectorSubtractionAB.b.count == n &&
                vectorSubtractionCD.c.count == n &&
                vectorSubtractionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorSubtractionAB.a.withUnsafeBufferPointer { a in
                    vectorSubtractionAB.b.withUnsafeBufferPointer { b in
                        vectorSubtractionCD.c.withUnsafeBufferPointer { c in
                            vectorSubtractionCD.d.withUnsafeBufferPointer { d in
                                vDSP_vsbsbmD(a.baseAddress!, 1,
                                             b.baseAddress!, 1,
                                             c.baseAddress!, 1,
                                             d.baseAddress!, 1,
                                             r.baseAddress!, 1,
                                             vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: e[i] = (a[i] + b[i]) * (c[i] - d[i])    vDSP_vasbm
    
    /// Populates `result` with the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, single-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter vectorSubtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(vectorAddition: (a: R, b: S),
                                               vectorSubtraction: (c: T, d: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n &&
                vectorSubtraction.c.count == n &&
                vectorSubtraction.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        vectorSubtraction.c.withUnsafeBufferPointer { c in
                            vectorSubtraction.d.withUnsafeBufferPointer { d in
                                vDSP_vasbm(a.baseAddress!, 1,
                                           b.baseAddress!, 1,
                                           c.baseAddress!, 1,
                                           d.baseAddress!, 1,
                                           r.baseAddress!, 1,
                                           vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, double-precision.
    ///
    /// - Parameter vectorAddition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter vectorSubtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(vectorAddition: (a: R, b: S),
                                               vectorSubtraction: (c: T, d: U),
                                               result: inout V)
        where
        R: _ContiguousCollection,
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorAddition.a.count == n &&
                vectorAddition.b.count == n &&
                vectorSubtraction.c.count == n &&
                vectorSubtraction.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorAddition.a.withUnsafeBufferPointer { a in
                    vectorAddition.b.withUnsafeBufferPointer { b in
                        vectorSubtraction.c.withUnsafeBufferPointer { c in
                            vectorSubtraction.d.withUnsafeBufferPointer { d in
                                vDSP_vasbmD(a.baseAddress!, 1,
                                            b.baseAddress!, 1,
                                            c.baseAddress!, 1,
                                            d.baseAddress!, 1,
                                            r.baseAddress!, 1,
                                            vDSP_Length(n))
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: d[n] = a[n]*b + c              vDSP_vsmsa
    
    /// Populates `result` with the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, single-precision.
    ///
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c` in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(vectorScalarMultiplication: (a: U, b: Float),
                                 _ scalar: Float,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                        withUnsafePointer(to: scalar) { c in
                            vDSP_vsmsa(a.baseAddress!, 1,
                                       b,
                                       c,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, double-precision.
    ///
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c`in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(vectorScalarMultiplication: (a: U, b: Double),
                                 _ scalar: Double,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                        withUnsafePointer(to: scalar) { c in
                            vDSP_vsmsaD(a.baseAddress!, 1,
                                        b,
                                        c,
                                        r.baseAddress!, 1,
                                        vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: D[n] = A[n]*B - C[n];       vDSP_vsmsb
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the vector and scalar in `vectorScalarMultiplication`,
    /// single-precision.
    ///
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vector: T,
                                         fromVectorScalarMultiplication vectorScalarMultiplication: (a: U, b: Float),
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            precondition(vector.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vsmsb(a.baseAddress!, 1,
                                       b,
                                       c.baseAddress!, 1,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the vector and scalar in `vectorScalarMultiplication`,
    /// double-precision.
    ///
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vectorScalarMultiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vector: T,
                                         fromVectorScalarMultiplication vectorScalarMultiplication: (a: U, b: Double),
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorScalarMultiplication.a.count == n)
            precondition(vector.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorScalarMultiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: vectorScalarMultiplication.b) { b in
                        vector.withUnsafeBufferPointer { c in
                            vDSP_vsmsbD(a.baseAddress!, 1,
                                        b,
                                        c.baseAddress!, 1,
                                        r.baseAddress!, 1,
                                        vDSP_Length(n))
                        }
                    }
                }
            }
    }
}

