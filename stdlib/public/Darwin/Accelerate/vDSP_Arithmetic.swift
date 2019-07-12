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

// Vector-vector and vector-scalar arithmetic

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: c[i] = a[i] + b                                vDSP_vsadd
    
    /// Returns the elementwise sum of `vector` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Returns: The `c` in `c[i] = a[i] + b`.
    @inlinable
    public static func add<U>(_ scalar: Float,
                              _ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(scalar,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b`.
    @inlinable
    public static func add<U, V>(_ scalar: Float,
                                 _ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vector` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Returns: The `c` in `c[i] = a[i] + b`.
    @inlinable
    public static func add<U>(_ scalar: Double,
                              _ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(scalar,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `b` in `c[i] = a[i] + b`.
    /// - Parameter vector: the `a` in `c[i] = a[i] + b`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b`.
    @inlinable
    public static func add<U, V>(_ scalar: Double,
                                 _ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add<T, U>(_ vectorA: T,
                                 _ vectorB: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                add(vectorA,
                    vectorB,
                    result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add<T, U, V>(_ vectorA: T,
                                    _ vectorB: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add<T, U>(_ vectorA: T,
                                 _ vectorB: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                add(vectorA,
                    vectorB,
                    result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] + b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] + b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] + b[i]`.
    @inlinable
    public static func add<T, U, V>(_ vectorA: T,
                                    _ vectorB: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract<T, U>(_ vectorA: U,
                                      _ vectorB: T) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                subtract(vectorA,
                         vectorB,
                         result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract<T, U, V>(_ vectorA: U,
                                         _ vectorB: T,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract<T, U>(_ vectorA: U,
                                      _ vectorB: T) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                subtract(vectorA,
                         vectorB,
                         result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] - b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] - b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] - b[i]`.
    @inlinable
    public static func subtract<T, U, V>(_ vectorA: U,
                                         _ vectorB: T,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of `vector` and `scalar
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Returns: The `c` in `c[i] = a[i] * b`.
    @inlinable
    public static func multiply<U>(_ scalar: Float,
                                   _ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(scalar,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of `vector` and `scalar
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b`.
    @inlinable
    public static func multiply<U, V>(_ scalar: Float,
                                      _ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of `vector` and `scalar
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Returns: The `c` in `c[i] = a[i] * b`.
    @inlinable
    public static func multiply<U>(_ scalar: Double,
                                   _ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(scalar,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of `vector` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] * b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] * b`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b`.
    @inlinable
    public static func multiply<U, V>(_ scalar: Double,
                                      _ vector: U,
                                      result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<T, U>(_ vectorA: T,
                                      _ vectorB: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                multiply(vectorA,
                         vectorB,
                         result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of `vectorA` and `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<T, U, V>(_ vectorA: T,
                                         _ vectorB: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<T, U>(_ vectorA: T,
                                      _ vectorB: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                multiply(vectorA,
                         vectorB,
                         result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of `vectorA` and `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] * b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] * b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] * b[i]`.
    @inlinable
    public static func multiply<T, U, V>(_ vectorA: T,
                                         _ vectorB: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `vector` by `scalar`,
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Returns: The `c` in `c[i] = a[i] / b`
    @inlinable
    public static func divide<U>(_ vector: U,
                                 _ scalar: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                divide(vector,
                       scalar,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `vector` by `scalar`,
    /// single-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b`
    @inlinable
    public static func divide<U, V>(_ vector: U,
                                    _ scalar: Float,
                                    result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `vector` by `scalar`,
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Returns: The `c` in `c[i] = a[i] / b`
    @inlinable
    public static func divide<U>(_ vector: U,
                                 _ scalar: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                divide(vector,
                       scalar,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `vector` by `scalar`,
    /// double-precision.
    ///
    /// - Parameter vector: the `a` in `c[i] = a[i] / b`.
    /// - Parameter scalar: the `b` in `c[i] = a[i] / b`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b`
    @inlinable
    public static func divide<U, V>(_ vector: U,
                                    _ scalar: Double,
                                    result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `scalar` by `vector`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Returns: The `c` in `c[i] = a / b[i]`.
    @inlinable
    public static func divide<U>(_ scalar: Float,
                                 _ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                divide(scalar,
                       vector,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `scalar` by `vector`,
    /// single-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a / b[i]`.
    @inlinable
    public static func divide<U, V>(_ scalar: Float,
                                    _ vector: U,
                                    result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `scalar` by `vector`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Returns: The `c` in `c[i] = a / b[i]`.
    @inlinable
    public static func divide<U>(_ scalar: Double,
                                 _ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                divide(scalar,
                       vector,
                       result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `scalar` by `vector`,
    /// double-precision.
    ///
    /// - Parameter scalar: the `a` in `c[i] = a / b[i]`.
    /// - Parameter vector: the `b` in `c[i] = a / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a / b[i]`.
    @inlinable
    public static func divide<U, V>(_ scalar: Double,
                                    _ vector: U,
                                    result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `vectorA` by `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<T, U>(_ vectorA: T,
                                    _ vectorB: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                divide(vectorA,
                       vectorB,
                       result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `vectorA` by `vectorB`,
    /// single-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<T, U, V>(_ vectorA: T,
                                       _ vectorB: U,
                                       result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise division of `vectorA` by `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Returns: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<T, U>(_ vectorA: T,
                                    _ vectorB: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                divide(vectorA,
                       vectorB,
                       result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise division of `vectorA` by `vectorB`,
    /// double-precision.
    ///
    /// - Parameter vectorA: the `a` in `c[i] = a[i] / b[i]`.
    /// - Parameter vectorB: the `b` in `c[i] = a[i] / b[i]`.
    /// - Parameter result: The `c` in `c[i] = a[i] / b[i]`.
    @inlinable
    public static func divide<T, U, V>(_ vectorA: T,
                                       _ vectorB: U,
                                       result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    @inlinable
    public static func addSubtract<S, T, U, V>(_ vectorA: S,
                                               _ vectorB: T,
                                               addResult: inout U,
                                               subtractResult: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateMutableBuffer,
        V: AccelerateMutableBuffer,
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
    @inlinable
    public static func addSubtract<S, T, U, V>(_ vectorA: S,
                                               _ vectorB: T,
                                               addResult: inout U,
                                               subtractResult: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateMutableBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the sum of the vectors in `addition` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Returns: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inlinable
    public static func multiply<T, U>(addition: (a: T, b: U),
                                      _ scalar: Float) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: addition.a.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         scalar,
                         result: &buffer)
                
                initializedCount = addition.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inlinable
    public static func multiply<T, U, V>(addition: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the sum of the vectors in `addition` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Returns: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inlinable
    public static func multiply<T, U>(addition: (a: T, b: U),
                                      _ scalar: Double) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: addition.a.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         scalar,
                         result: &buffer)
                
                initializedCount = addition.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] + b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] + b[i]) * c`.
    @inlinable
    public static func multiply<T, U, V>(addition: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Returns: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U>(addition: (a: S, b: T),
                                         _ vector: U) -> [Float]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U, V>(addition: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Returns: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U>(addition: (a: S, b: T),
                                         _ vector: U) -> [Double]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the sum of the vectors in `addition` and `vector`,
    /// double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] + b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = (a[i] + b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U, V>(addition: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Returns: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inlinable
    public static func multiply<T, U>(subtraction: (a: T, b: U),
                                      _ scalar: Float) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: subtraction.a.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtraction,
                         scalar,
                         result: &buffer)
                
                initializedCount = subtraction.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inlinable
    public static func multiply<T, U, V>(subtraction: (a: T, b: U),
                                         _ scalar: Float,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Returns: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inlinable
    public static func multiply<T, U>(subtraction: (a: T, b: U),
                                      _ scalar: Double) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: subtraction.a.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtraction,
                         scalar,
                         result: &buffer)
                
                initializedCount = subtraction.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `scalar`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c`.
    /// - Parameter scalar: the `c` in `d[i] = `(a[i] - b[i]) * c`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c`.
    @inlinable
    public static func multiply<T, U, V>(subtraction: (a: T, b: U),
                                         _ scalar: Double,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Returns: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U>(subtraction: (a: S, b: T),
                                         _ vector: U) -> [Float]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtraction,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// single-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U, V>(subtraction: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Returns: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U>(subtraction: (a: S, b: T),
                                         _ vector: U) -> [Double]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtraction,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of the difference of the vectors in `subtraction` and `vector`,
    /// double-precision.
    ///
    /// - Parameter subtraction: the `a` and `b` in `d[i] = (a[i] - b[i]) * c[i]`.
    /// - Parameter vector: the `c` in `d[i] = `(a[i] - b[i]) * c[i]`.
    /// - Parameter result: The `d` in `d[i] = `(a[i] - b[i]) * c[i]`.
    @inlinable
    public static func multiply<S, T, U, V>(subtraction: (a: S, b: T),
                                            _ vector: U,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `scalar`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Returns: the `d` in `d[i] = a[i]*b[i] + c`.
    @inlinable
    public static func add<T, U>(multiplication: (a: T, b: U),
                                 _ scalar: Float) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: multiplication.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    scalar,
                    result: &buffer)
                
                initializedCount = multiplication.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `scalar`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inlinable
    public static func add<T, U, V>(multiplication: (a: T, b: U),
                                    _ scalar: Float,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `scalar`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Returns: the `d` in `d[i] = a[i]*b[i] + c`.
    @inlinable
    public static func add<T, U>(multiplication: (a: T, b: U),
                                 _ scalar: Double) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: multiplication.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    scalar,
                    result: &buffer)
                
                initializedCount = multiplication.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `scalar`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter scalar: the `c` in `d[i] = a[i]*b[i] + c`.
    /// - Parameter result: the `d` in `d[i] = a[i]*b[i] + c`.
    @inlinable
    public static func add<T, U, V>(multiplication: (a: T, b: U),
                                    _ scalar: Double,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inlinable
    public static func add<T, U>(multiplication: (a: T, b: Float),
                                 _ vector: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inlinable
    public static func add<T, U, V>(multiplication: (a: T, b: Float),
                                    _ vector: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inlinable
    public static func add<T, U>(multiplication: (a: T, b: Double),
                                 _ vector: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b) + c[i]`.
    @inlinable
    public static func add<T, U, V>(multiplication: (a: T, b: Double),
                                    _ vector: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inlinable
    public static func add<S, T, U>(multiplication: (a: S, b: T),
                                    _ vector: U) -> [Float]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inlinable
    public static func add<S, T, U, V>(multiplication: (a: S, b: T),
                                       _ vector: U,
                                       result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inlinable
    public static func add<S, T, U>(multiplication: (a: S, b: T),
                                    _ vector: U) -> [Double]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) + c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) + c[i]`.
    @inlinable
    public static func add<S, T, U, V>(multiplication: (a: S, b: T),
                                       _ vector: U,
                                       result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inlinable
    public static func subtract<S, T, U>(multiplication: (a: T, b: U),
                                         _ vector: S) -> [Float]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplication,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inlinable
    public static func subtract<S, T, U, V>(multiplication: (a: T, b: U),
                                            _ vector: S,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Returns: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inlinable
    public static func subtract<S, T, U>(multiplication: (a: T, b: U),
                                         _ vector: S) -> [Double]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplication,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the two vectors in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter vector: the `c` in `d[i] = (a[i] * b[i]) - c[i]`.
    /// - Parameter result: the `d` in `d[i] = (a[i] * b[i]) - c[i]`.
    @inlinable
    public static func subtract<S, T, U, V>(multiplication: (a: T, b: U),
                                            _ vector: S,
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of two elementwise
    /// vector-scalar products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Returns: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inlinable
    public static func add<T, U>(multiplication multiplicationAB: (a: T, b: Float),
                                 multiplication multiplicationCD: (c: U, d: Float)) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplicationAB,
                    multiplication: multiplicationCD,
                    result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-scalar products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inlinable
    public static func add<T, U, V>(multiplication multiplicationAB: (a: T, b: Float),
                                    multiplication multiplicationCD: (c: U, d: Float),
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of two elementwise
    /// vector-scalar products, double-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Returns: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inlinable
    public static func add<T, U>(multiplication multiplicationAB: (a: T, b: Double),
                                 multiplication multiplicationCD: (c: U, d: Double)) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplicationAB,
                    multiplication: multiplicationCD,
                    result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-scalar products, double-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b) + (c[i] * d)`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b) + (c[i] * d)`.
    @inlinable
    public static func add<T, U, V>(multiplication multiplicationAB: (a: T, b: Double),
                                    multiplication multiplicationCD: (c: U, d: Double),
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inlinable
    public static func add<R, S, T, U>(multiplication multiplicationAB: (a: R, b: S),
                                       multiplication multiplicationCD: (c: T, d: U)) -> [Float]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplicationAB,
                    multiplication: multiplicationCD,
                    result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inlinable
    public static func add<R, S, T, U, V>(multiplication multiplicationAB: (a: R, b: S),
                                          multiplication multiplicationCD: (c: T, d: U),
                                          result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inlinable
    public static func add<R, S, T, U>(multiplication multiplicationAB: (a: R, b: S),
                                       multiplication multiplicationCD: (c: T, d: U)) -> [Double]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplicationAB,
                    multiplication: multiplicationCD,
                    result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of two elementwise
    /// vector-vector products, double-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) + (c[i] * d[i])`.
    @inlinable
    public static func add<R, S, T, U, V>(multiplication multiplicationAB: (a: R, b: S),
                                          multiplication multiplicationCD: (c: T, d: U),
                                          result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of two elementwise
    /// vector-vector sums, single-precision.
    ///
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inlinable
    public static func multiply<S, T, U>(addition additionAB: (a: S, b: T),
                                         addition additionCD: (c: U, d: U)) -> [Float]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: additionAB.a.count) {
                buffer, initializedCount in
                
                multiply(addition: additionAB,
                         addition: additionCD,
                         result: &buffer)
                
                initializedCount = additionAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector sums, single-precision.
    ///
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inlinable
    public static func multiply<S, T, U, V>(addition additionAB: (a: S, b: T),
                                            addition additionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of two elementwise
    /// vector-vector sums, double-precision.
    ///
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inlinable
    public static func multiply<S, T, U>(addition additionAB: (a: S, b: T),
                                         addition additionCD: (c: U, d: U)) -> [Double]
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: additionAB.a.count) {
                buffer, initializedCount in
                
                multiply(addition: additionAB,
                         addition: additionCD,
                         result: &buffer)
                
                initializedCount = additionAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector sums, double-precision.
    ///
    /// - Parameter additionAB: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter additionCD: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] + d[i])`.
    @inlinable
    public static func multiply<S, T, U, V>(addition additionAB: (a: S, b: T),
                                            addition additionCD: (c: U, d: U),
                                            result: inout V)
        where
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inlinable
    public static func subtract<R, S, T, U>(multiplication multiplicationAB: (a: T, b: U),
                                            multiplication multiplicationCD: (c: R, d: S)) -> [Float]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplicationAB,
                         multiplication: multiplicationCD,
                         result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of two elementwise
    /// vector-vector products, single-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inlinable
    public static func subtract<R, S, T, U, V>(multiplication multiplicationAB: (a: T, b: U),
                                               multiplication multiplicationCD: (c: R, d: S),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of two elementwise
    /// vector-vector products, double-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inlinable
    public static func subtract<R, S, T, U>(multiplication multiplicationAB: (a: T, b: U),
                                            multiplication multiplicationCD: (c: R, d: S)) -> [Double]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: multiplicationAB.a.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplicationAB,
                         multiplication: multiplicationCD,
                         result: &buffer)
                
                initializedCount = multiplicationAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of two elementwise
    /// vector-vector products, double-precision.
    ///
    /// - Parameter multiplicationAB: the `a` and `b` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter multiplicationCD: the `c` and `d` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] * b[i]) - (c[i] * d[i])`.
    @inlinable
    public static func subtract<R, S, T, U, V>(multiplication multiplicationAB: (a: T, b: U),
                                               multiplication multiplicationCD: (c: R, d: S),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of two elementwise
    /// vector-vector differences, single-precision.
    ///
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U>(subtraction subtractionAB: (a: R, b: S),
                                            subtraction subtractionCD: (c: T, d: U)) -> [Float]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: subtractionAB.a.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtractionAB,
                         subtraction: subtractionCD,
                         result: &buffer)
                
                initializedCount = subtractionAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector differences, single-precision.
    ///
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U, V>(subtraction subtractionAB: (a: R, b: S),
                                               subtraction subtractionCD: (c: T, d: U),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of two elementwise
    /// vector-vector differences, double-precision.
    ///
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U>(subtraction subtractionAB: (a: R, b: S),
                                            subtraction subtractionCD: (c: T, d: U)) -> [Double]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: subtractionAB.a.count) {
                buffer, initializedCount in
                
                multiply(subtraction: subtractionAB,
                         subtraction: subtractionCD,
                         result: &buffer)
                
                initializedCount = subtractionAB.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of two elementwise
    /// vector-vector differences, double-precision.
    ///
    /// - Parameter subtractionAB: the `a` and `b` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter subtractionCD: the `c` and `d` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] - b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U, V>(subtraction subtractionAB: (a: R, b: S),
                                               subtraction subtractionCD: (c: T, d: U),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U>(addition: (a: R, b: S),
                                            subtraction: (c: T, d: U)) -> [Float]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Float,
        S.Element == Float, T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: addition.a.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         subtraction: subtraction,
                         result: &buffer)
                
                initializedCount = addition.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, single-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U, V>(addition: (a: R, b: S),
                                               subtraction: (c: T, d: U),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Returns: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U>(addition: (a: R, b: S),
                                            subtraction: (c: T, d: U)) -> [Double]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Double,
        S.Element == Double, T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: addition.a.count) {
                buffer, initializedCount in
                
                multiply(addition: addition,
                         subtraction: subtraction,
                         result: &buffer)
                
                initializedCount = addition.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise product of an elementwise
    /// vector-vector sum and an elementwise vector-vector sum, double-precision.
    ///
    /// - Parameter addition: the `a` and `b` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter subtraction: the `c` and `d` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    /// - Parameter result: the `e` in `e[i] = (a[i] + b[i]) * (c[i] - d[i])`.
    @inlinable
    public static func multiply<R, S, T, U, V>(addition: (a: R, b: S),
                                               subtraction: (c: T, d: U),
                                               result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c` in `d[n] = a[n]*b + c`.
    /// - Returns: the `e` in `d[n] = a[n]*b + c`.
    @inlinable
    public static func add<U>(multiplication: (a: U, b: Float),
                              _ scalar: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: multiplication.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    scalar,
                    result: &buffer)
                
                initializedCount =  multiplication.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c` in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inlinable
    public static func add<U, V>(multiplication: (a: U, b: Float),
                                 _ scalar: Float,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c` in `d[n] = a[n]*b + c`.
    /// - Returns: the `e` in `d[n] = a[n]*b + c`.
    @inlinable
    public static func add<U>(multiplication: (a: U, b: Double),
                              _ scalar: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: multiplication.a.count) {
                buffer, initializedCount in
                
                add(multiplication: multiplication,
                    scalar,
                    result: &buffer)
                
                initializedCount =  multiplication.a.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise sum of an elementwise
    /// vector-scalar product and aa scalar value, double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `d[n] = a[n]*b + c`.
    /// - Parameter scalar: the `c`in `d[n] = a[n]*b + c`.
    /// - Parameter result: the `e` in `d[n] = a[n]*b + c`.
    @inlinable
    public static func add<U, V>(multiplication: (a: U, b: Double),
                                 _ scalar: Double,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Returns: the `d` in `D[n] = A[n]*B - C[n]`.
    @inlinable
    public static func subtract<T, U>(multiplication: (a: U, b: Float),
                                      _ vector: T) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplication,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// single-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inlinable
    public static func subtract<T, U, V>(multiplication: (a: U, b: Float),
                                         _ vector: T,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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
    
    /// Returns the elementwise difference of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Returns: the `d` in `D[n] = A[n]*B - C[n]`.
    @inlinable
    public static func subtract<T, U>(multiplication: (a: U, b: Double),
                                      _ vector: T) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                subtract(multiplication: multiplication,
                         vector,
                         result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elementwise difference of `vector`
    /// and the product of the vector and scalar in `multiplication`,
    /// double-precision.
    ///
    /// - Parameter vector: the `c` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter multiplication: the `a` and `b` in `D[n] = A[n]*B - C[n]`.
    /// - Parameter result: the `d` in `D[n] = A[n]*B - C[n]`.
    @inlinable
    public static func subtract<T, U, V>(multiplication: (a: U, b: Double),
                                         _ vector: T,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
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

