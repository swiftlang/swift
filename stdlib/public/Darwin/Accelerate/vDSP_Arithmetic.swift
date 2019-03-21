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
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vectorA: U,
                                         _ vectorB: T,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorB.withUnsafeBufferPointer { b in
                    vectorA.withUnsafeBufferPointer { a in
                        vDSP_vsub(b.baseAddress!, 1,
                                  a.baseAddress!, 1,
                                  r.baseAddress!, 1,
                                  vDSP_Length(n))
                    }
                }
            }
    }
    
    /// Populates `result` with the elementwise difference of `vector` and `fromVector`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(_ vectorA: U,
                                         _ vectorB: T,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vectorA.count == n && vectorB.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                vectorB.withUnsafeBufferPointer { b in
                    vectorA.withUnsafeBufferPointer { a in
                        vDSP_vsubD(b.baseAddress!, 1,
                                   a.baseAddress!, 1,
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
                                    _ scalar: Float,
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
                                    _ scalar: Double,
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
    /// - Parameter vectorA: the `i1` in `o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter vectorB: the `i0` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
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
                    vectorA.withUnsafeBufferPointer { i1 in
                        vectorB.withUnsafeBufferPointer { i0 in
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
    /// - Parameter vectorA: the `i1` in `o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
    /// - Parameter vectorB: the `i0` in o0[i] = i1[i] + i0[i]; o1[i] = i1[i] - i0[i]`.
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
                    vectorA.withUnsafeBufferPointer { i1 in
                        vectorB.withUnsafeBufferPointer { i0 in
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
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `byScalar`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(addition: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(addition.a.count == n &&
                addition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `byScalar`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(addition: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(addition.a.count == n &&
                addition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(addition: (a: S, b: T),
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
            precondition(addition.a.count == n &&
                addition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(addition: (a: S, b: T),
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
            precondition(addition.a.count == n &&
                addition.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(subtraction: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(subtraction.a.count == n &&
                subtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtraction.a.withUnsafeBufferPointer { a in
                    subtraction.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<T, U, V>(subtraction: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(subtraction.a.count == n &&
                subtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtraction.a.withUnsafeBufferPointer { a in
                    subtraction.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(subtraction: (a: S, b: T),
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
            precondition(subtraction.a.count == n &&
                subtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtraction.a.withUnsafeBufferPointer { a in
                    subtraction.b.withUnsafeBufferPointer { b in
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
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(subtraction: (a: S, b: T),
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
            precondition(subtraction.a.count == n &&
                subtraction.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtraction.a.withUnsafeBufferPointer { a in
                    subtraction.b.withUnsafeBufferPointer { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication: (a: T, b: U),
                                    _ scalar: Float,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication: (a: T, b: U),
                                    _ scalar: Double,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication: (a: T, b: Float),
                                    _ vector: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(multiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    vector.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: multiplication.b) { b in
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
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication: (a: T, b: Double),
                                    _ vector: U,
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(multiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    vector.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: multiplication.b) { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<S, T, U, V>(multiplication: (a: S, b: T),
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
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<S, T, U, V>(multiplication: (a: S, b: T),
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
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<S, T, U, V>(multiplication: (a: T, b: U),
                                            _ vector: S,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<S, T, U, V>(multiplication: (a: T, b: U),
                                            _ vector: S,
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(multiplication.a.count == n &&
                multiplication.b.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    multiplication.b.withUnsafeBufferPointer { b in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication multiplicationAB: (a: T, b: Float),
                                    multiplication multiplicationCD: (c: U, d: Float),
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(multiplicationAB.a.count == n &&
                multiplicationCD.c.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationCD.c.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: multiplicationAB.b) { b in
                            withUnsafePointer(to: multiplicationCD.d) { d in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<T, U, V>(multiplication multiplicationAB: (a: T, b: Double),
                                    multiplication multiplicationCD: (c: U, d: Double),
                                    result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(multiplicationAB.a.count == n &&
                multiplicationCD.c.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationCD.c.withUnsafeBufferPointer { c in
                        withUnsafePointer(to: multiplicationAB.b) { b in
                            withUnsafePointer(to: multiplicationCD.d) { d in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<R, S, T, U, V>(multiplication multiplicationAB: (a: R, b: S),
                                          multiplication multiplicationCD: (c: T, d: U),
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
            precondition(multiplicationAB.a.count == n &&
                multiplicationAB.b.count == n &&
                multiplicationCD.c.count == n &&
                multiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationAB.b.withUnsafeBufferPointer { b in
                        multiplicationCD.c.withUnsafeBufferPointer { c in
                            multiplicationCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<R, S, T, U, V>(multiplication multiplicationAB: (a: R, b: S),
                                          multiplication multiplicationCD: (c: T, d: U),
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
            precondition(multiplicationAB.a.count == n &&
                multiplicationAB.b.count == n &&
                multiplicationCD.c.count == n &&
                multiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationAB.b.withUnsafeBufferPointer { b in
                        multiplicationCD.c.withUnsafeBufferPointer { c in
                            multiplicationCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(addition additionAB: (a: S, b: T),
                                            addition additionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Float, T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(additionAB.a.count == n &&
                additionAB.b.count == n &&
                additionCD.c.count == n &&
                additionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                additionAB.a.withUnsafeBufferPointer { a in
                    additionAB.b.withUnsafeBufferPointer { b in
                        additionCD.c.withUnsafeBufferPointer { c in
                            additionCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<S, T, U, V>(addition additionAB: (a: S, b: T),
                                            addition additionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: _ContiguousCollection,
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        S.Element == Double, T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(additionAB.a.count == n &&
                additionAB.b.count == n &&
                additionCD.c.count == n &&
                additionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                additionAB.a.withUnsafeBufferPointer { a in
                    additionAB.b.withUnsafeBufferPointer { b in
                        additionCD.c.withUnsafeBufferPointer { c in
                            additionCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<R, S, T, U, V>(multiplication multiplicationAB: (a: T, b: U),
                                               multiplication multiplicationCD: (c: R, d: S),
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
            precondition(multiplicationAB.a.count == n &&
                multiplicationAB.b.count == n &&
                multiplicationCD.c.count == n &&
                multiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationAB.b.withUnsafeBufferPointer { b in
                        multiplicationCD.c.withUnsafeBufferPointer { c in
                            multiplicationCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<R, S, T, U, V>(multiplication multiplicationAB: (a: T, b: U),
                                               multiplication multiplicationCD: (c: R, d: S),
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
            precondition(multiplicationAB.a.count == n &&
                multiplicationAB.b.count == n &&
                multiplicationCD.c.count == n &&
                multiplicationCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplicationAB.a.withUnsafeBufferPointer { a in
                    multiplicationAB.b.withUnsafeBufferPointer { b in
                        multiplicationCD.c.withUnsafeBufferPointer { c in
                            multiplicationCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(subtraction subtractionAB: (a: R, b: S),
                                               subtraction subtractionCD: (c: T, d: U),
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
            precondition(subtractionAB.a.count == n &&
                subtractionAB.b.count == n &&
                subtractionCD.c.count == n &&
                subtractionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtractionAB.a.withUnsafeBufferPointer { a in
                    subtractionAB.b.withUnsafeBufferPointer { b in
                        subtractionCD.c.withUnsafeBufferPointer { c in
                            subtractionCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(subtraction subtractionAB: (a: R, b: S),
                                               subtraction subtractionCD: (c: T, d: U),
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
            precondition(subtractionAB.a.count == n &&
                subtractionAB.b.count == n &&
                subtractionCD.c.count == n &&
                subtractionCD.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                subtractionAB.a.withUnsafeBufferPointer { a in
                    subtractionAB.b.withUnsafeBufferPointer { b in
                        subtractionCD.c.withUnsafeBufferPointer { c in
                            subtractionCD.d.withUnsafeBufferPointer { d in
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
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(addition: (a: R, b: S),
                                               subtraction: (c: T, d: U),
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
            precondition(addition.a.count == n &&
                addition.b.count == n &&
                subtraction.c.count == n &&
                subtraction.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
                        subtraction.c.withUnsafeBufferPointer { c in
                            subtraction.d.withUnsafeBufferPointer { d in
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
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func multiply<R, S, T, U, V>(addition: (a: R, b: S),
                                               subtraction: (c: T, d: U),
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
            precondition(addition.a.count == n &&
                addition.b.count == n &&
                subtraction.c.count == n &&
                subtraction.d.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                addition.a.withUnsafeBufferPointer { a in
                    addition.b.withUnsafeBufferPointer { b in
                        subtraction.c.withUnsafeBufferPointer { c in
                            subtraction.d.withUnsafeBufferPointer { d in
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
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c` in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(multiplication: (a: U, b: Float),
                                 _ scalar: Float,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            let n = result.count
            precondition(multiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: multiplication.b) { b in
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
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c`in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func add<U, V>(multiplication: (a: U, b: Double),
                                 _ scalar: Double,
                                 result: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            let n = result.count
            precondition(multiplication.a.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: multiplication.b) { b in
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
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(multiplication: (a: U, b: Float),
                                         _ vector: T,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Float,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(multiplication.a.count == n)
            precondition(vector.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: multiplication.b) { b in
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
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func subtract<T, U, V>(multiplication multiplication: (a: U, b: Double),
                                         _ vector: T,
                                         result: inout V)
        where
        T: _ContiguousCollection,
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        T.Element == Double,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(multiplication.a.count == n)
            precondition(vector.count == n)
            
            result.withUnsafeMutableBufferPointer { r in
                multiplication.a.withUnsafeBufferPointer { a in
                    withUnsafePointer(to: multiplication.b) { b in
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

