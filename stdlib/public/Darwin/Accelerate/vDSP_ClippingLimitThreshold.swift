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
    
    // MARK: Clip
    
    /// Returns the elements of `vector` clipped to the specified range. Single-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    @inlinable
    public static func clip<U>(_ vector: U,
                               to bounds: ClosedRange<Float>) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                clip(vector,
                     to: bounds,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elements of `vector` clipped to the specified range. Single-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Parameter result: The clipped result.
    @inlinable
    public static func clip<U, V>(_ vector: U,
                                  to bounds: ClosedRange<Float>,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: bounds.lowerBound) { lowerBound in
                withUnsafePointer(to: bounds.upperBound) { upperBound in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_vclip(v.baseAddress!, 1,
                                       lowerBound,
                                       upperBound,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Returns the elements of `vector` clipped to the specified range. Double-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    @inlinable
    public static func clip<U>(_ vector: U,
                               to bounds: ClosedRange<Double>) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                clip(vector,
                     to: bounds,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elements of `vector` clipped to the specified range. Double-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Parameter result: The clipped result.
    @inlinable
    public static func clip<U, V>(_ vector: U,
                                  to bounds: ClosedRange<Double>,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: bounds.lowerBound) { lowerBound in
                withUnsafePointer(to: bounds.upperBound) { upperBound in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_vclipD(v.baseAddress!, 1,
                                        lowerBound,
                                        upperBound,
                                        r.baseAddress!, 1,
                                        vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: Inverted Clip
    
    /// Returns the elements of `vector` inverted-clipped to the specified range. Single-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    ///
    /// This function performs the following operation where `A` is `vector`, `B` is `bounds.lowerBound`,
    /// `C` is `bounds.upperBound`, and `D` is the inverted clip result:
    /// ```
    /// for (int n = 0; n < N; ++n) {
    ///     if (A[n] <= *B || A[n] >= *C)
    ///         D[n] = A[n];
    ///     else if (A[n] < 0)
    ///         D[n] = *B;
    ///     else
    ///         D[n] = *C;
    /// }
    /// ```
    @inlinable
    public static func invertedClip<U>(_ vector: U,
                                       to bounds: ClosedRange<Float>) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                invertedClip(vector,
                             to: bounds,
                             result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elements of `vector` inverted-clipped to the specified range. Single-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Parameter result: The clipped result.
    ///
    /// This function performs the following operation where `A` is `vector`, `B` is `bounds.lowerBound`,
    /// `C` is `bounds.upperBound`, and `D` is the inverted clip result:
    /// ```
    /// for (int n = 0; n < N; ++n) {
    ///     if (A[n] <= *B || A[n] >= *C)
    ///         D[n] = A[n];
    ///     else if (A[n] < 0)
    ///         D[n] = *B;
    ///     else
    ///         D[n] = *C;
    /// }
    /// ```
    @inlinable
    public static func invertedClip<U, V>(_ vector: U,
                                          to bounds: ClosedRange<Float>,
                                          result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: bounds.lowerBound) { lowerBound in
                withUnsafePointer(to: bounds.upperBound) { upperBound in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_viclip(v.baseAddress!, 1,
                                        lowerBound,
                                        upperBound,
                                        r.baseAddress!, 1,
                                        vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Returns the elements of `vector` inverted-clipped to the specified range. Double-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    ///
    /// This function performs the following operation where `A` is `vector`, `B` is `bounds.lowerBound`,
    /// `C` is `bounds.upperBound`, and `D` is the inverted clip result:
    /// ```
    /// for (int n = 0; n < N; ++n) {
    ///     if (A[n] <= *B || A[n] >= *C)
    ///         D[n] = A[n];
    ///     else if (A[n] < 0)
    ///         D[n] = *B;
    ///     else
    ///         D[n] = *C;
    /// }
    /// ```
    @inlinable
    public static func invertedClip<U>(_ vector: U,
                                       to bounds: ClosedRange<Double>) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                invertedClip(vector,
                             to: bounds,
                             result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with the elements of `vector` inverted-clipped to the specified range. Double-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Parameter result: The clipped result.
    ///
    /// This function performs the following operation where `A` is `vector`, `B` is `bounds.lowerBound`,
    /// `C` is `bounds.upperBound`, and `D` is the inverted clip result:
    /// ```
    /// for (int n = 0; n < N; ++n) {
    ///     if (A[n] <= *B || A[n] >= *C)
    ///         D[n] = A[n];
    ///     else if (A[n] < 0)
    ///         D[n] = *B;
    ///     else
    ///         D[n] = *C;
    /// }
    /// ```
    @inlinable
    public static func invertedClip<U, V>(_ vector: U,
                                          to bounds: ClosedRange<Double>,
                                          result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: bounds.lowerBound) { lowerBound in
                withUnsafePointer(to: bounds.upperBound) { upperBound in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_viclipD(v.baseAddress!, 1,
                                         lowerBound,
                                         upperBound,
                                         r.baseAddress!, 1,
                                         vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    // MARK: Vector threshold.
  
    public enum ThresholdRule<T: BinaryFloatingPoint> {
        /// Returns threshold if input is less than threshold; otherwise input value.
        case clampToThreshold
        /// Returns zero if input is less than threshold; otherwise input value.
        case zeroFill
        /// Returns `-x` if input is less than threshold; otherwise `+x`.
        case signedConstant(_ x: T)
    }
    
    /// Returns the elements of `vector` after applying a specified thresholding rule. Single-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    @inlinable
    public static func threshold<U>(_ vector: U,
                                    to lowerBound: Float,
                                    with rule: ThresholdRule<Float>) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                threshold(vector,
                          to: lowerBound,
                          with: rule,
                          result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with with the corresponding value in `vector` after applying a specified thresholding rule. Single-precision.
    ///
    /// This function supports the following rules:
    ///
    /// * `clampToThreshold` - Returns `lowerBound` if input is less than `lowerBound`; otherwise returns input value.
    /// * `zeroFill` - Returns `0` if input is less than `lowerBound`; otherwise returns input value.
    /// * `signedConstant(x)` - returns `-x` if input is less than `lowerBound`; otherwise `+x`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter lowerBound: Low clipping threshold.
    /// - Parameter rule: The thresholding rule.
    /// - Parameter result: The threshold result.
    public static func threshold<U, V>(_ vector: U,
                                       to lowerBound: Float,
                                       with rule: ThresholdRule<Float>,
                                       result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: lowerBound) { lowerBound in
                result.withUnsafeMutableBufferPointer { dest in
                    vector.withUnsafeBufferPointer { src in
                        switch rule {
                        case .clampToThreshold:
                            vDSP_vthr(src.baseAddress!, 1,
                                      lowerBound,
                                      dest.baseAddress!, 1,
                                      vDSP_Length(n))
                        case .zeroFill:
                            vDSP_vthres(src.baseAddress!, 1,
                                        lowerBound,
                                        dest.baseAddress!, 1,
                                        vDSP_Length(n))
                        case .signedConstant(let x):
                            vDSP_vthrsc(src.baseAddress!, 1,
                                        lowerBound,
                                        [x],
                                        dest.baseAddress!, 1,
                                        vDSP_Length(n))
                            
                        }
                    }
                }
            }
    }
    
    /// Returns the elements of `vector` after applying a specified thresholding rule. Double-precision.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter bounds: Clipping threshold.
    /// - Returns: The clipped result.
    @inlinable
    public static func threshold<U>(_ vector: U,
                                    to lowerBound: Double,
                                    with rule: ThresholdRule<Double>) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                threshold(vector,
                          to: lowerBound,
                          with: rule,
                          result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Populates `result` with with the corresponding value in `vector` after applying a specified thresholding rule. Double-precision.
    ///
    /// This function supports the following rules:
    ///
    /// * `clampToThreshold` - Returns `lowerBound` if input is less than `lowerBound`; otherwise returns input value.
    /// * `zeroFill` - Returns `0` if input is less than `lowerBound`; otherwise returns input value.
    /// * `signedConstant(x)` - returns `-x` if input is less than `lowerBound`; otherwise `+x`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter lowerBound: Low clipping threshold.
    /// - Parameter rule: The thresholding rule.
    /// - Parameter result: The threshold result.
    public static func threshold<U, V>(_ vector: U,
                                       to lowerBound: Double,
                                       with rule: ThresholdRule<Double>,
                                       result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: lowerBound) { lowerBound in
                result.withUnsafeMutableBufferPointer { dest in
                    vector.withUnsafeBufferPointer { src in
                        switch rule {
                        case .clampToThreshold:
                            vDSP_vthrD(src.baseAddress!, 1,
                                       lowerBound,
                                       dest.baseAddress!, 1,
                                       vDSP_Length(n))
                        case .zeroFill:
                            vDSP_vthresD(src.baseAddress!, 1,
                                         lowerBound,
                                         dest.baseAddress!, 1,
                                         vDSP_Length(n))
                        case .signedConstant(let x):
                            vDSP_vthrscD(src.baseAddress!, 1,
                                         lowerBound,
                                         [x],
                                         dest.baseAddress!, 1,
                                         vDSP_Length(n))
                            
                        }
                    }
                }
            }
    }
    
    // MARK: Limit (vlim)
    
    /// Vector test limit; single-precision.
    ///
    /// Compares values from source vector to `limit`. For inputs greater than or equal to `limit`, `outputConstant` is written to `result`. For inputs less than `limit`, the negated value of `outputConstant` is written to `result`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter limit: Limit.
    /// - Parameter x: Value written to result.
    /// - Returns: The clipped result.
    @inlinable
    public static func limit<U>(_ vector: U,
                                limit: Float,
                                withOutputConstant outputConstant: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                vDSP.limit(vector,
                           limit: limit,
                           withOutputConstant: outputConstant,
                           result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Vector test limit; single-precision.
    ///
    /// Compares values from source vector to `limit`. For inputs greater than or equal to `limit`, `outputConstant` is written to `result`. For inputs less than `limit`, the negated value of `outputConstant` is written to `result`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter limit: Limit.
    /// - Parameter x: Value written to result.
    /// - Parameter result: The clipped result.
    @inlinable
    public static func limit<U, V>(_ vector: U,
                                   limit: Float,
                                   withOutputConstant outputConstant: Float,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: limit) { limit in
                withUnsafePointer(to: outputConstant) { x in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_vlim(v.baseAddress!, 1,
                                      limit,
                                      x,
                                      r.baseAddress!, 1,
                                      vDSP_Length(n))
                        }
                    }
                }
            }
    }
    
    /// Vector test limit; double-precision.
    ///
    /// Compares values from source vector to `limit`. For inputs greater than or equal to `limit`, `outputConstant` is written to `result`. For inputs less than `limit`, the negated value of `outputConstant` is written to `result`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter limit: Limit.
    /// - Parameter x: Value written to result.
    /// - Returns: The clipped result.
    @inlinable
    public static func limit<U>(_ vector: U,
                                limit: Double,
                                withOutputConstant outputConstant: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                vDSP.limit(vector,
                           limit: limit,
                           withOutputConstant: outputConstant,
                           result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Vector test limit; double-precision.
    ///
    /// Compares values from source vector to `limit`. For inputs greater than or equal to `limit`, `outputConstant` is written to `result`. For inputs less than `limit`, the negated value of `outputConstant` is written to `result`.
    ///
    /// - Parameter vector: Source vector.
    /// - Parameter limit: Limit.
    /// - Parameter x: Value written to result.
    /// - Parameter result: The clipped result.
    @inlinable
    public static func limit<U, V>(_ vector: U,
                                   limit: Double,
                                   withOutputConstant outputConstant: Double,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = result.count
            precondition(vector.count == n)
            
            withUnsafePointer(to: limit) { limit in
                withUnsafePointer(to: outputConstant) { x in
                    result.withUnsafeMutableBufferPointer { r in
                        vector.withUnsafeBufferPointer { v in
                            vDSP_vlimD(v.baseAddress!, 1,
                                       limit,
                                       x,
                                       r.baseAddress!, 1,
                                       vDSP_Length(n))
                        }
                    }
                }
            }
    }
}
