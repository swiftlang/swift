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

//  TODO: Support vectors with `count > Int32.max` by calling vvceilf in a
//  loop, processing 0x40000000 elements at a go.

// Array-Oriented Arithmetic and Auxiliary Functions

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vForce {
    
    // MARK: Ceiling
    
    /// Returns the ceiling of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func ceil<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                ceil(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the ceiling of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func ceil<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvceilf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the ceiling of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func ceil<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                ceil(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the ceiling of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func ceil<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvceil(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Floor
    
    /// Returns the floor of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func floor<U>(_ vector: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                floor(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the floor of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func floor<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvfloorf(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the floor of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func floor<U>(_ vector: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                floor(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the floor of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func floor<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvfloor(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Copy sign
    
    /// Returns the sign of each element in `signs` to the corresponding element in `magnitudes`, single-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Returns: Output values.
    @inlinable
    public static func copysign<U, V>(magnitudes: U,
                                      signs: V) -> [Float]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(magnitudes.count == signs.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: magnitudes.count) {
                buffer, initializedCount in
                
                copysign(magnitudes: magnitudes,
                         signs: signs,
                         result: &buffer)
                
                initializedCount = magnitudes.count
            }
            
            return result
    }
    
    /// Copies the sign of each element in `signs` to the corresponding element in `magnitudes`, writing the result to `result`, single-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Parameter result: Output values.
    @inlinable
    public static func copysign<T, U, V>(magnitudes: T,
                                         signs: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(magnitudes.count == signs.count && signs.count == result.count)
            
            var n = Int32(magnitudes.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                magnitudes.withUnsafeBufferPointer { mag in
                    signs.withUnsafeBufferPointer { sgn in
                        vvcopysignf(dest.baseAddress!,
                                    mag.baseAddress!,
                                    sgn.baseAddress!,
                                    &n)
                    }
                }
            }
    }
    
    /// Returns the sign of each element in `signs` to the corresponding element in `magnitudes`, double-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Returns: Output values.
    @inlinable
    public static func copysign<U, V>(magnitudes: U,
                                      signs: V) -> [Double]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(magnitudes.count == signs.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: magnitudes.count) {
                buffer, initializedCount in
                
                copysign(magnitudes: magnitudes,
                         signs: signs,
                         result: &buffer)
                
                initializedCount = magnitudes.count
            }
            
            return result
    }
    
    /// Copies the sign of each element in `signs` to the corresponding element in `magnitudes`, writing the result to `result`, double-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Parameter result: Output values.
    @inlinable
    public static func copysign<T, U, V>(magnitudes: T,
                                         signs: U,
                                         result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(magnitudes.count == signs.count && signs.count == result.count)
            
            var n = Int32(magnitudes.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                magnitudes.withUnsafeBufferPointer { mag in
                    signs.withUnsafeBufferPointer { sgn in
                        vvcopysign(dest.baseAddress!,
                                   mag.baseAddress!,
                                   sgn.baseAddress!,
                                   &n)
                    }
                }
            }
    }
    
    // `vvdiv(_:_:_:_:)` omitted, we have `vDSP_vdiv` in vDSP already.
    // `vvfabs(_:_:_:)` omitted, we have `vDSP_zvabs` in vDSP already.
    
    // MARK: Modulus
    
    /// Returns the remainder of the elements in `dividends` divided by the the elements in `divisors` using truncating division; single-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Returns: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is the integer formed by rounding `a/b` towards zero, so `abs(r) < abs(b)` and `r` has the same sign as `a`.
    @inlinable
    public static func truncatingRemainder<U, V>(dividends: U,
                                                 divisors: V) -> [Float]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(dividends.count == divisors.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: dividends.count) {
                buffer, initializedCount in
                
                truncatingRemainder(dividends: dividends,
                                    divisors: divisors,
                                    result: &buffer)
                
                initializedCount = dividends.count
            }
            
            return result
    }
    
    /// Calculates the remainder of the elements in `dividends` divided by the the elements
    /// in `divisors` using truncating division; single-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Parameter result: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is the integer formed by rounding `a/b` towards zero, so `abs(r) < abs(b)` and `r` has the same sign as `a`.
    @inlinable
    public static func truncatingRemainder<T, U, V>(dividends: T,
                                                    divisors: U,
                                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(dividends.count == divisors.count && divisors.count == result.count)
            
            var n = Int32(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                dividends.withUnsafeBufferPointer { a in
                    divisors.withUnsafeBufferPointer { b in
                        vvfmodf(dest.baseAddress!,
                                a.baseAddress!,
                                b.baseAddress!,
                                &n)
                    }
                }
            }
    }
    
    /// Returns the remainder of the elements in `dividends` divided by the the elements in `divisors` using truncating division; double-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Returns: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is the integer formed by rounding `a/b` towards zero, so `abs(r) < abs(b)` and `r` has the same sign as `a`.
    @inlinable
    public static func truncatingRemainder<U, V>(dividends: U,
                                                 divisors: V) -> [Double]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(dividends.count == divisors.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: dividends.count) {
                buffer, initializedCount in
                
                truncatingRemainder(dividends: dividends,
                                    divisors: divisors,
                                    result: &buffer)
                
                initializedCount = dividends.count
            }
            
            return result
    }
    
    /// Calculates the remainder of the elements in `dividends` divided by the the elements
    /// in `divisors` using truncating division; single-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Parameter result: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is the integer formed by rounding `a/b` towards zero, so `abs(r) < abs(b)` and `r` has the same sign as `a`.
    @inlinable
    public static func truncatingRemainder<T, U, V>(dividends: T,
                                                    divisors: U,
                                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(dividends.count == divisors.count && divisors.count == result.count)
            
            var n = Int32(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                dividends.withUnsafeBufferPointer { a in
                    divisors.withUnsafeBufferPointer { b in
                        vvfmod(dest.baseAddress!,
                               a.baseAddress!,
                               b.baseAddress!,
                               &n)
                    }
                }
            }
    }
    
    // MARK: Remainder
    
    /// Returns the remainder after dividing each element in `dividends` by the corresponding element in `divisors`, single-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Returns: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is `a/b` rounded to the nearest integer, so `abs(r) <= abs(b/2)`.
    @inlinable
    public static func remainder<U, V>(dividends: U,
                                       divisors: V) -> [Float]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(dividends.count == divisors.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: dividends.count) {
                buffer, initializedCount in
                
                remainder(dividends: dividends,
                          divisors: divisors,
                          result: &buffer)
                
                initializedCount = dividends.count
            }
            
            return result
    }
    
    /// Calculates the remainder after dividing each element in `dividends` by the corresponding element in `divisors`, single-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Parameter result: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is `a/b` rounded to the nearest integer, so `abs(r) <= abs(b/2)`.
    @inlinable
    public static func remainder<T, U, V>(dividends: T,
                                          divisors: U,
                                          result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(dividends.count == divisors.count && divisors.count == result.count)
            
            var n = Int32(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                dividends.withUnsafeBufferPointer { a in
                    divisors.withUnsafeBufferPointer { b in
                        vvremainderf(dest.baseAddress!,
                                     a.baseAddress!,
                                     b.baseAddress!,
                                     &n)
                    }
                }
            }
    }
    
    /// Returns the remainder after dividing each element in `dividends` by the corresponding element in `divisors`, double-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Returns: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is `a/b` rounded to the nearest integer, so `abs(r) <= abs(b/2)`.
    @inlinable
    public static func remainder<U, V>(dividends: U,
                                       divisors: V) -> [Double]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(dividends.count == divisors.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: dividends.count) {
                buffer, initializedCount in
                
                remainder(dividends: dividends,
                          divisors: divisors,
                          result: &buffer)
                
                initializedCount = dividends.count
            }
            
            return result
    }
    
    /// Calculates the remainder after dividing each element in `dividends` by the corresponding element in `divisors`, double-precision.
    ///
    /// - Parameter dividends: Input dividends.
    /// - Parameter divisors: Input divisors.
    /// - Parameter result: Output values.
    ///
    /// For each corresponding `a` from `dividends` and `b` from `divisors`, the result `r` satisfies:
    ///     a = bq + r
    /// where q is `a/b` rounded to the nearest integer, so `abs(r) <= abs(b/2)`.
    @inlinable
    public static func remainder<T, U, V>(dividends: T,
                                          divisors: U,
                                          result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(dividends.count == divisors.count && divisors.count == result.count)
            
            var n = Int32(result.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                dividends.withUnsafeBufferPointer { a in
                    divisors.withUnsafeBufferPointer { b in
                        vvremainder(dest.baseAddress!,
                                    a.baseAddress!,
                                    b.baseAddress!,
                                    &n)
                    }
                }
            }
    }
    
    // MARK: Integer Truncation
    
    /// Returns the integer truncation of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func trunc<U>(_ vector: U)  -> [Float]
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
    
    /// Calculates the integer truncation of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvintf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the integer truncation of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func trunc<U>(_ vector: U)  -> [Double]
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
    
    /// Calculates the integer truncation of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func trunc<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvint(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
    
    // MARK: Nearest Integer
    
    /// Returns the nearest integer to each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func nearestInteger<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                nearestInteger(vector,
                               result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the nearest integer to each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func nearestInteger<U, V>(_ vector: U,
                                            result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvnintf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the nearest integer to each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func nearestInteger<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                nearestInteger(vector,
                               result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the nearest integer to each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func nearestInteger<U, V>(_ vector: U,
                                            result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvnint(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Reciprocal Square Root
    
    /// Returns the reciprocal square root of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func rsqrt<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                rsqrt(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the reciprocal square root of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func rsqrt<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvrsqrtf(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the reciprocal square root of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func rsqrt<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                rsqrt(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the reciprocal square root of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func rsqrt<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvrsqrt(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Square Root
    
    /// Returns the square root of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sqrt<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sqrt(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the square root of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sqrt<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsqrtf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the square root of each element in `vector`, double-precision.
    ///
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sqrt<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sqrt(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the square root of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sqrt<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsqrt(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Reciprocal
    
    /// Returns the reciprocal of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func reciprocal<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                reciprocal(vector,
                           result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the reciprocal of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func reciprocal<U, V>(_ vector: U,
                                        result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvrecf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the reciprocal of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func reciprocal<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                reciprocal(vector,
                           result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the reciprocal of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func reciprocal<U, V>(_ vector: U,
                                        result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvrec(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
}

// Array-Oriented Exponential and Logarithmic Functions

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vForce {
    
    // MARK: Exponential
    
    /// Returns e raised to the power of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func exp<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                exp(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates e raised to the power of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func exp<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexpf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns e raised to the power of each element minus one in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func expm1<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                expm1(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates e raised to the power of each element minus one in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func expm1<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexpm1f(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns 2 raised to the power of each element  minus one  in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func exp2<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                exp2(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates 2 raised to the power of each element  minus one  in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func exp2<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexp2f(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns e raised to the power of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func exp<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                exp(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates e raised to the power of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func exp<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexp(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
    
    /// Returns e raised to the power of each element minus one in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func expm1<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                expm1(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates e raised to the power of each element  minus one  in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func expm1<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexpm1(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns 2 raised to the power of each element  minus one  in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func exp2<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                exp2(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates 2 raised to the power of each element  minus one  in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func exp2<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvexp2(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Logarithm
    
    /// Returns the base two logarithm of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func log2<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                log2(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the base two logarithm of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func log2<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlog2f(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the base ten logarithm of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func log10<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                log10(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the base ten logarithm of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func log10<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlog10f(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the base two logarithm of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func log2<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                log2(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the base two logarithm of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func log2<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlog2(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the base ten logarithm of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func log10<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                log10(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the ten logarithm of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func log10<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlog10(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Unbiased Exponent
    
    /// Returns the unbiased exponent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func logb<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                logb(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the unbiased exponent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    ///
    /// This function calculates `floor(log2(vector))`.
    @inlinable
    public static func logb<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlogbf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the unbiased exponent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func logb<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                logb(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the unbiased exponent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    ///
    /// This function calculates `floor(log2(vector))`.
    @inlinable
    public static func logb<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvlogb(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
}

// Array-Oriented Power Functions

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vForce {
    
    // MARK: Power
    
    /// Returns each element in `bases` rasied to the power of the corresponding element in `exponents`, single-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Returns: Output values.
    @inlinable
    public static func pow<U, V>(bases: U,
                                 exponents: V) -> [Float]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(bases.count == exponents.count)
            
            let result = Array<Float>(unsafeUninitializedCapacity: exponents.count) {
                buffer, initializedCount in
                
                pow(bases: bases,
                    exponents: exponents,
                    result: &buffer)
                
                initializedCount = exponents.count
            }
            
            return result
    }
    
    /// Calculates each element in `bases` rasied to the power of the corresponding element in `exponents`, writing the result to `result`, single-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Parameter result: Output values.
    @inlinable
    public static func pow<T, U, V>(bases: T,
                                    exponents: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(bases.count == exponents.count && exponents.count == result.count)
            
            var n = Int32(bases.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                bases.withUnsafeBufferPointer { bases in
                    exponents.withUnsafeBufferPointer { exponents in
                        vvpowf(dest.baseAddress!,
                               exponents.baseAddress!,
                               bases.baseAddress!,
                               &n)
                    }
                }
            }
    }
    
    /// Returns each element in `bases` rasied to the power of the corresponding element in `exponents`, double-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Returns: Output values.
    @inlinable
    public static func pow<U, V>(bases: U,
                                 exponents: V) -> [Double]
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(bases.count == exponents.count)
            
            let result = Array<Double>(unsafeUninitializedCapacity: exponents.count) {
                buffer, initializedCount in
                
                pow(bases: bases,
                    exponents: exponents,
                    result: &buffer)
                
                initializedCount = exponents.count
            }
            
            return result
    }
    
    /// Calculates each element in `bases` rasied to the power of the corresponding element in `exponents`, writing the result to `result`, double-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Parameter result: Output values.
    @inlinable
    public static func pow<T, U, V>(bases: T,
                                    exponents: U,
                                    result: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double,
        U.Element == Double,
        V.Element == Double {
            
            precondition(bases.count == exponents.count && exponents.count == result.count)
            
            var n = Int32(bases.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                bases.withUnsafeBufferPointer { bases in
                    exponents.withUnsafeBufferPointer { exponents in
                        vvpow(dest.baseAddress!,
                              exponents.baseAddress!,
                              bases.baseAddress!,
                              &n)
                    }
                }
            }
    }
    
    
}

// Array-Oriented Trigonometric Functions

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vForce {
    
    // MARK: Sine
    
    /// Returns the sine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sin<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sin(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sin<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsinf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the sine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sin<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sin(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sin<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsin(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
    
    /// Returns the sine of pi multiplied of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sinPi<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sinPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the sine of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sinPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsinpif(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the sine of pi multiplied of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sinPi<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sinPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the sine of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sinPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsinpi(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Cosine
    
    /// Returns the cosine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cos<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cos(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cos<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcosf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the cosine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cos<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cos(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cos<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcos(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
    
    /// Returns the cosine of pi multiplied of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cosPi<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cosPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the cosine of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cosPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcospif(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the cosine of pi multiplied of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cosPi<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cosPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the cosine of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cosPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcospi(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Cosine & Sine
    
    /// Calculates the sine and cosine of each element in `vector`, writing the results to `sinresult` and `cosresult` respectively, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter sinresult: Output sine values.
    /// - Parameter cosresult: Output cosine values.
    @inlinable
    public static func sincos<T, U, V>(_ vector: T,
                                       sinResult: inout U,
                                       cosResult: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateMutableBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(vector.count == sinResult.count && sinResult.count == cosResult.count)
            
            var n = Int32(vector.count)
            
            sinResult.withUnsafeMutableBufferPointer { sinDest in
                cosResult.withUnsafeMutableBufferPointer { cosDest in
                    vector.withUnsafeBufferPointer { src in
                        vvsincosf(sinDest.baseAddress!,
                                  cosDest.baseAddress!,
                                  src.baseAddress!,
                                  &n)
                    }
                }
            }
    }
    
    /// Calculates the sine and cosine of each element in `vector`, writing the results to `sinresult` and `cosresult` respectively, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter sinresult: Output sine values.
    /// - Parameter cosresult: Output cosine values.
    @inlinable
    public static func sincos<T, U, V>(_ vector: T,
                                       sinResult: inout U,
                                       cosResult: inout V)
        where
        T: AccelerateBuffer,
        U: AccelerateMutableBuffer,
        V: AccelerateMutableBuffer,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(vector.count == sinResult.count && sinResult.count == cosResult.count)
            
            var n = Int32(vector.count)
            
            sinResult.withUnsafeMutableBufferPointer { sinDest in
                cosResult.withUnsafeMutableBufferPointer { cosDest in
                    vector.withUnsafeBufferPointer { src in
                        vvsincos(sinDest.baseAddress!,
                                 cosDest.baseAddress!,
                                 src.baseAddress!,
                                 &n)
                    }
                }
            }
    }
    
    // MARK: Tan
    
    /// Returns the tangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tan<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tan(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tan<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtanf(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    /// Returns the tangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tan<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tan(vector,
                    result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    
    /// Calculates the tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tan<U, V>(_ vector: U,
                                 result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtan(dest.baseAddress!,
                          src.baseAddress!,
                          &n)
                }
            }
    }
    
    /// Returns the tangent of pi multiplied of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tanPi<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tanPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the tangent of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tanPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtanpif(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the tangent of pi multiplied of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tanPi<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tanPi(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the tangent of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tanPi<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtanpi(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Arcsine
    
    /// Returns the arcsine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func asin<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                asin(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arcsine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func asin<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvasinf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the arcsine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func asin<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                asin(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arcsine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func asin<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvasin(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Arccosine
    
    /// Returns the arccosine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func acos<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                acos(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arccosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func acos<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvacosf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the arccosine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func acos<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                acos(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arccosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func acos<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvacos(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Arctangent
    
    /// Returns the arctangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func atan<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                atan(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arctangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func atan<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvatanf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the arctangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func atan<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                atan(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the arctangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func atan<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvatan(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
}

// Array-Oriented Hyperbolic Functions

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vForce {
    
    // MARK: Hyperbolic Sine
    
    /// Returns the hyperbolic sine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sinh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sinh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sinh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsinhf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the hyperbolic sine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func sinh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                sinh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func sinh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvsinh(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Hyperbolic Cosine
    
    /// Returns the hyperbolic cosine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cosh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cosh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cosh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcoshf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the hyperbolic cosine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func cosh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                cosh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func cosh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvcosh(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Hyperbolic Tangent
    
    /// Returns the hyperbolic tangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tanh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tanh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tanh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtanhf(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    /// Returns the hyperbolic tangent of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func tanh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                tanh(vector,
                     result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the hyperbolic tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func tanh<U, V>(_ vector: U,
                                  result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvtanh(dest.baseAddress!,
                           src.baseAddress!,
                           &n)
                }
            }
    }
    
    // MARK: Inverse Hyperbolic Sine
    
    /// Returns the inverse hyperbolic sine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func asinh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                asinh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func asinh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvasinhf(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the inverse hyperbolic sine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func asinh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                asinh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func asinh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvasinh(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Inverse Hyperbolic Cosine
    
    /// Returns the inverse hyperbolic cosine of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func acosh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                acosh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func acosh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvacoshf(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the inverse hyperbolic cosine of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func acosh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                acosh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func acosh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvacosh(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
    
    // MARK: Inverse Hyperbolic Tangent
    
    /// Returns the inverse hyperbolic tangent of each element in `vector`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func atanh<U>(_ vector: U)  -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                atanh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func atanh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvatanhf(dest.baseAddress!,
                             src.baseAddress!,
                             &n)
                }
            }
    }
    
    /// Returns the inverse hyperbolic tangent of each element in `vector`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Returns: Output values.
    @inlinable
    public static func atanh<U>(_ vector: U)  -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                buffer, initializedCount in
                
                atanh(vector,
                      result: &buffer)
                
                initializedCount = vector.count
            }
            
            return result
    }
    
    /// Calculates the inverse hyperbolic tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inlinable
    public static func atanh<U, V>(_ vector: U,
                                   result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    vvatanh(dest.baseAddress!,
                            src.baseAddress!,
                            &n)
                }
            }
    }
}
