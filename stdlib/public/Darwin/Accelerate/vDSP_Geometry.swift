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
    
    // MARK Dot product
    
    /// Returns the dot or scalar product of vectors A and B; single-precision.
    ///
    /// - Parameter vectorA: Single-precision real input vector A.
    /// - Parameter vectorB: Single-precision real input vector B.
    /// - Returns: The dot product of vectors A and B.
    @inlinable
    public static func dot<U>(_ vectorA: U,
                              _ vectorB: U) -> Float
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            precondition(vectorA.count == vectorB.count)
            
            let n = vDSP_Length(vectorA.count)
            var result = Float.nan
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    
                    vDSP_dotpr(a.baseAddress!, 1,
                               b.baseAddress!, 1,
                               &result, n)
                    
                }
            }
            
            return result
    }
    
    /// Returns the dot or scalar product of vectors A and B; double-precision.
    ///
    /// - Parameter vectorA: Double-precision real input vector A.
    /// - Parameter vectorB: Double-precision real input vector B.
    /// - Returns: The dot product of vectors A and B.
    @inlinable
    public static func dot<U>(_ vectorA: U,
                              _ vectorB: U) -> Double
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            precondition(vectorA.count == vectorB.count)
            
            let n = vDSP_Length(vectorA.count)
            var result = Double.nan
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    
                    vDSP_dotprD(a.baseAddress!, 1,
                                b.baseAddress!, 1,
                                &result, n)
                    
                }
            }
            
            return result
    }
    
    // MARK: Distance
    
    /// Returns the hypotenuse of right-angled triangles with sides that are the lengths of
    /// corresponding elements in vectors `x` and `y`; single-precision.
    ///
    /// - Parameter x: The `x` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter y: The `y` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter result: The `z` in `z[i] = sqrt(x[i]² + y[i]²)`.
    @inlinable
    public static func hypot<U, V>(_ x: U,
                                   _ y: V) -> [Float]
        where
        U: AccelerateBuffer,
        V: AccelerateBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(x.count == y.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: x.count) {
                buffer, initializedCount in
                
                hypot(x, y,
                     result: &buffer)
                
                initializedCount = x.count
            }
            
            return result
    }
    
    /// Calculates the hypotenuse of right-angled triangles with sides that are the lengths of
    /// corresponding elements in vectors `x` and `y`; single-precision.
    ///
    /// - Parameter x: The `x` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter y: The `y` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter result: The `z` in `z[i] = sqrt(x[i]² + y[i]²)`.
    @inlinable
    public static func hypot<T, U, V>(_ x: T,
                                      _ y: U,
                                      result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(x.count == y.count && y.count == result.count)
            let n = vDSP_Length(result.count)
            
            x.withUnsafeBufferPointer { a in
                y.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        vDSP_vdist(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   dest.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
    
    /// Returns the hypotenuse of right-angled triangles with sides that are the lengths of
    /// corresponding elements in vectors `x` and `y`; double-precision.
    ///
    /// - Parameter x: The `x` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter y: The `y` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter result: The `z` in `z[i] = sqrt(x[i]² + y[i]²)`.
    @inlinable
    public static func hypot<U, V>(_ x: U,
                                   _ y: V) -> [Double]
        where
        U: AccelerateBuffer,
        V: AccelerateBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(x.count == y.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: x.count) {
                buffer, initializedCount in
                
                hypot(x, y,
                      result: &buffer)
                
                initializedCount = x.count
            }
            
            return result
    }
    
    /// Calculates the hypotenuse of right-angled triangles with sides that are the lengths of
    /// corresponding elements in vectors `x` and `y`; double-precision.
    ///
    /// - Parameter x: The `x` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter y: The `y` in `z[i] = sqrt(x[i]² + y[i]²)`.
    /// - Parameter result: The `z` in `z[i] = sqrt(x[i]² + y[i]²)`.
    @inlinable
    public static func hypot<T, U, V>(_ x: T,
                                      _ y: U,
                                      result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(x.count == y.count && y.count == result.count)
            let n = vDSP_Length(result.count)
            
            x.withUnsafeBufferPointer { a in
                y.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        vDSP_vdistD(a.baseAddress!, 1,
                                    b.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    // MARK: Pythagoras
    
    /// Returns the hypotenuse of right-angled triangles with sides that are the differences of
    /// corresponding values in x0 and x1, and y0 and y1. Single-precision.
    ///
    /// - Parameter x0: The `x0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter x1: The `x1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y0: The `y0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y1: The `y1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter result: The `z` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    @inlinable
    public static func hypot<R, S, T, U>(x0: R, x1: S,
                                            y0: T, y1: U) -> [Float]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Float, S.Element == Float,
        T.Element == Float, U.Element == Float {
            
            precondition(x0.count == x1.count)
            precondition(y0.count == y1.count)
            precondition(x0.count == y0.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: x0.count) {
                buffer, initializedCount in
                
                hypot(x0: x0, x1: x1,
                      y0: y0, y1: y1,
                      result: &buffer)
                
                initializedCount = x0.count
            }
            
            return result
    }
    
    /// Calculates the hypotenuse of right-angled triangles with sides that are the differences of
    /// corresponding values in x0 and x1, and y0 and y1. Single-precision.
    ///
    /// - Parameter x0: The `x0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter x1: The `x1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y0: The `y0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y1: The `y1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter result: The `z` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    @inlinable
    public static func hypot<R, S, T, U, V>(x0: R, x1: S,
                                            y0: T, y1: U,
                                            result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        R.Element == Float, S.Element == Float,
        T.Element == Float, U.Element == Float,
        V.Element == Float {
            
            precondition(x0.count == x1.count && x0.count == result.count)
            precondition(y0.count == y1.count && y0.count == result.count)
            
            let n = vDSP_Length(result.count)
            
            x0.withUnsafeBufferPointer { a in
                x1.withUnsafeBufferPointer { c in
                    y0.withUnsafeBufferPointer { b in
                        y1.withUnsafeBufferPointer { d in
                            result.withUnsafeMutableBufferPointer { dest in
                                vDSP_vpythg(a.baseAddress!, 1,
                                            b.baseAddress!, 1,
                                            c.baseAddress!, 1,
                                            d.baseAddress!, 1,
                                            dest.baseAddress!, 1,
                                            n)
                            }
                        }
                    }
                }
            }
    }
    
    /// Returns the hypotenuse of right-angled triangles with sides that are the differences of
    /// corresponding values in x0 and x1, and y0 and y1. Double-precision.
    ///
    /// - Parameter x0: The `x0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter x1: The `x1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y0: The `y0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y1: The `y1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter result: The `z` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    @inlinable
    public static func hypot<R, S, T, U>(x0: R, x1: S,
                                            y0: T, y1: U) -> [Double]
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        R.Element == Double, S.Element == Double,
        T.Element == Double, U.Element == Double {
            
            precondition(x0.count == x1.count)
            precondition(y0.count == y1.count)
            precondition(x0.count == y0.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: x0.count) {
                buffer, initializedCount in
                
                hypot(x0: x0, x1: x1,
                      y0: y0, y1: y1,
                      result: &buffer)
                
                initializedCount = x0.count
            }
            
            return result
    }
    
    /// Calculates the hypotenuse of right-angled triangles with sides that are the differences of
    /// corresponding values in x0 and x1, and y0 and y1. Double-precision.
    ///
    /// - Parameter x0: The `x0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter x1: The `x1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y0: The `y0` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter y1: The `y1` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    /// - Parameter result: The `z` in `z[i] = sqrt( (x0[i] - x1[i])² + (y0[i] - y1[i])² )`.
    @inlinable
    public static func hypot<R, S, T, U, V>(x0: R, x1: S,
                                            y0: T, y1: U,
                                            result: inout V)
        where
        R: AccelerateBuffer,
        S: AccelerateBuffer,
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        R.Element == Double, S.Element == Double,
        T.Element == Double, U.Element == Double,
        V.Element == Double {
            
            precondition(x0.count == x1.count && x0.count == result.count)
            precondition(y0.count == y1.count && y0.count == result.count)
            
            let n = vDSP_Length(result.count)
            
            x0.withUnsafeBufferPointer { a in
                x1.withUnsafeBufferPointer { c in
                    y0.withUnsafeBufferPointer { b in
                        y1.withUnsafeBufferPointer { d in
                            result.withUnsafeMutableBufferPointer { dest in
                                vDSP_vpythgD(a.baseAddress!, 1,
                                             b.baseAddress!, 1,
                                             c.baseAddress!, 1,
                                             d.baseAddress!, 1,
                                             dest.baseAddress!, 1,
                                             n)
                            }
                        }
                    }
                }
            }
    }
    
    // MARK: Distance Squared
    
    /// Returns the distance squared between two points in `n` dimensional space. Single-precision.
    ///
    /// - Parameter pointA: First point in `n` dimensional space, where `n` is the collection count.
    /// - Parameter pointB: Second point in `n` dimensional space, where `n` is the collection count.
    /// - Returns: The distance squared between `pointA` and `pointB`.
    @inlinable
    public static func distanceSquared<U, V>(_ pointA: U,
                                             _ pointB: V) -> Float
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(pointA.count == pointB.count)
            
            let n = vDSP_Length(pointA.count)
            var result = Float.nan
            
            pointA.withUnsafeBufferPointer { a in
                pointB.withUnsafeBufferPointer { b in
                    vDSP_distancesq(a.baseAddress!, 1,
                                    b.baseAddress!, 1,
                                    &result,
                                    n)
                }
            }
            
            return result
    }
    
    /// Returns the distance squared between two points in `n` dimensional space. Double-precision.
    ///
    /// - Parameter pointA: First point in `n` dimensional space, where `n` is the collection count.
    /// - Parameter pointB: Second point in `n` dimensional space, where `n` is the collection count.
    /// - Returns: The distance squared between `pointA` and `pointB`.
    @inlinable
    public static func distanceSquared<U, V>(_ pointA: U,
                                             _ pointB: V) -> Double
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(pointA.count == pointB.count)
            
            let n = vDSP_Length(pointA.count)
            var result = Double.nan
            
            pointA.withUnsafeBufferPointer { a in
                pointB.withUnsafeBufferPointer { b in
                    vDSP_distancesqD(a.baseAddress!, 1,
                                     b.baseAddress!, 1,
                                     &result,
                                     n)
                }
            }
            
            return result
    }
    
}
