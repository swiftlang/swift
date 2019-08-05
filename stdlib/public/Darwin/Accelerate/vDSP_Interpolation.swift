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
    
    /// Vector linear interpolation between vectors; single-precision.
    ///
    /// - Parameter vectorA: The `A` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter vectorB: The `B` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter interpolationConstant: The `C` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Returns: The `D` in `D[n] = A[n] + C * (B[n] - A[n])`.
    @inlinable
    public static func linearInterpolate<T, U>(_ vectorA: T,
                                               _ vectorB: U,
                                               using interpolationConstant: Float) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float{
            
            let result = Array<Float>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                linearInterpolate(vectorA,
                                  vectorB,
                                  using: interpolationConstant,
                                  result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Vector linear interpolation between vectors; single-precision.
    ///
    /// - Parameter vectorA: The `A` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter vectorB: The `B` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter interpolationConstant: The `C` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter result: The `D` in `D[n] = A[n] + C * (B[n] - A[n])`.
    @inlinable
    public static func linearInterpolate<T, U, V>(_ vectorA: T,
                                                  _ vectorB: U,
                                                  using interpolationConstant: Float,
                                                  result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(vectorA.count == result.count)
            precondition(vectorB.count == result.count)
            let n = vDSP_Length(result.count)
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        
                        vDSP_vintb(a.baseAddress!, 1,
                                   b.baseAddress!, 1,
                                   [interpolationConstant],
                                   dest.baseAddress!, 1,
                                   n)
                    }
                }
            }
    }
    
    /// Vector linear interpolation between vectors; double-precision.
    ///
    /// - Parameter vectorA: The `A` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter vectorB: The `B` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter interpolationConstant: The `C` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Returns: The `D` in `D[n] = A[n] + C * (B[n] - A[n])`.
    @inlinable
    public static func linearInterpolate<T, U>(_ vectorA: T,
                                               _ vectorB: U,
                                               using interpolationConstant: Double) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double{
            
            let result = Array<Double>(unsafeUninitializedCapacity: vectorA.count) {
                buffer, initializedCount in
                
                linearInterpolate(vectorA,
                                  vectorB,
                                  using: interpolationConstant,
                                  result: &buffer)
                
                initializedCount = vectorA.count
            }
            
            return result
    }
    
    /// Vector linear interpolation between vectors; double-precision.
    ///
    /// - Parameter vectorA: The `A` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter vectorB: The `B` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter interpolationConstant: The `C` in `D[n] = A[n] + C * (B[n] - A[n])`.
    /// - Parameter result: The `D` in `D[n] = A[n] + C * (B[n] - A[n])`.
    @inlinable
    public static func linearInterpolate<T, U, V>(_ vectorA: T,
                                                  _ vectorB: U,
                                                  using interpolationConstant: Double,
                                                  result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(vectorA.count == result.count)
            precondition(vectorB.count == result.count)
            let n = vDSP_Length(result.count)
            
            vectorA.withUnsafeBufferPointer { a in
                vectorB.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        
                        vDSP_vintbD(a.baseAddress!, 1,
                                    b.baseAddress!, 1,
                                    [interpolationConstant],
                                    dest.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    /// Vector linear interpolation between neighboring elements; single-precision.
    ///
    /// This function interpolates between the elements of `vector` using the following:
    ///
    ///        for (n = 0; n < N; ++n) {
    ///            b = trunc(B[n]);
    ///            a = B[n] - b;
    ///            C[n] = A[b] + a * (A[b+1] - A[b]);
    ///        }
    ///
    /// Where `A` is the input vector, `B` is the control vector, and
    /// `C` is the output vector.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter controlVector: Vector that controls interpolation.
    /// - Returns: Output values.
    @inlinable
    public static func linearInterpolate<T, U>(elementsOf vector: T,
                                               using controlVector: U) -> [Float]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Float, U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: controlVector.count) {
                buffer, initializedCount in
                
                linearInterpolate(elementsOf: vector,
                                  using: controlVector,
                                  result: &buffer)
                
                initializedCount = controlVector.count
            }
            
            return result
    }
    
    /// Vector linear interpolation between neighboring elements; single-precision.
    ///
    /// This function interpolates between the elements of `vector` using the following:
    ///
    ///        for (n = 0; n < N; ++n) {
    ///            b = trunc(B[n]);
    ///            a = B[n] - b;
    ///            C[n] = A[b] + a * (A[b+1] - A[b]);
    ///        }
    ///
    /// Where `A` is the input vector, `B` is the control vector, and
    /// `C` is the output vector.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter controlVector: Vector that controls interpolation.
    /// - Parameter result: Output values.
    @inlinable
    public static func linearInterpolate<T, U, V>(elementsOf vector: T,
                                                  using controlVector: U,
                                                  result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(controlVector.count == result.count)
            
            let n = vDSP_Length(result.count)
            let m = vDSP_Length(vector.count)
            
            vector.withUnsafeBufferPointer { a in
                controlVector.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        vDSP_vlint(a.baseAddress!,
                                   b.baseAddress!, 1,
                                   dest.baseAddress!, 1,
                                   n, m)
                    }
                }
            }
    }
    
    /// Vector linear interpolation between neighboring elements; double-precision.
    ///
    /// This function interpolates between the elements of `vector` using the following:
    ///
    ///        for (n = 0; n < N; ++n) {
    ///            b = trunc(B[n]);
    ///            a = B[n] - b;
    ///            C[n] = A[b] + a * (A[b+1] - A[b]);
    ///        }
    ///
    /// Where `A` is the input vector, `B` is the control vector, and
    /// `C` is the output vector.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter controlVector: Vector that controls interpolation.
    /// - Returns: Output values.
    @inlinable
    public static func linearInterpolate<T, U>(elementsOf vector: T,
                                               using controlVector: U) -> [Double]
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        T.Element == Double, U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: controlVector.count) {
                buffer, initializedCount in
                
                linearInterpolate(elementsOf: vector,
                                  using: controlVector,
                                  result: &buffer)
                
                initializedCount = controlVector.count
            }
            
            return result
    }
    
    /// Vector linear interpolation between neighboring elements; double-precision.
    ///
    /// This function interpolates between the elements of `vector` using the following:
    ///
    ///        for (n = 0; n < N; ++n) {
    ///            b = trunc(B[n]);
    ///            a = B[n] - b;
    ///            C[n] = A[b] + a * (A[b+1] - A[b]);
    ///        }
    ///
    /// Where `A` is the input vector, `B` is the control vector, and
    /// `C` is the output vector.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter controlVector: Vector that controls interpolation.
    /// - Parameter result: Output values.
    @inlinable
    public static func linearInterpolate<T, U, V>(elementsOf vector: T,
                                                  using controlVector: U,
                                                  result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(controlVector.count == result.count)
            
            let n = vDSP_Length(result.count)
            let m = vDSP_Length(vector.count)
            
            vector.withUnsafeBufferPointer { a in
                controlVector.withUnsafeBufferPointer { b in
                    result.withUnsafeMutableBufferPointer { dest in
                        vDSP_vlintD(a.baseAddress!,
                                    b.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n, m)
                    }
                }
            }
    }
}
