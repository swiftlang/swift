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

import Accelerate.vecLib

// Top level enum for namespaced vDSP and vForce based functions
public enum vDSP {}

// Array-Oriented Arithmetic and Auxiliary Functions

extension vDSP {
    
    // MARK: Ceiling
    
    /// Calculates the ceiling of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func ceiling<U, V>(_ vector: U,
                              result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the ceiling of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func ceiling<U, V>(_ vector: U,
                              result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the floor of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func floor<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the floor of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func floor<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Copies the sign of each element in `signs` to the corresponding element in `magnitudes`, writing the result to `result`, single-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func copySign<T, U, V>(magnitudes: T,
                                  signs: U,
                                  result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Copies the sign of each element in `signs` to the corresponding element in `magnitudes`, writing the result to `result`, double-precision.
    ///
    /// - Parameter magnitudes: Input magnitudes.
    /// - Parameter signs: Input signs.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func copySign<T, U, V>(magnitudes: T,
                                  signs: U,
                                  result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the modulus after dividing each element in `numerators` by the corresponding element in `denominators`, single-precision.
    ///
    /// - Parameter numerators: Input numerators.
    /// - Parameter denominators: Input denominators.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func modulus<T, U, V>(numerators: T,
                                 denominators: U,
                                 result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(denominators.count == denominators.count && denominators.count == result.count)
            
            var n = Int32(numerators.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                numerators.withUnsafeBufferPointer { num in
                    denominators.withUnsafeBufferPointer { denom in
                        vvfmodf(dest.baseAddress!,
                                num.baseAddress!,
                                denom.baseAddress!,
                                &n)
                    }
                }
            }
    }
    
    /// Calculates the modulus after dividing each element in `numerators` by the corresponding element in `denominators`, double-precision.
    ///
    /// - Parameter numerators: Input numerators.
    /// - Parameter denominators: Input denominators.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func modulus<T, U, V>(numerators: T,
                                 denominators: U,
                                 result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(denominators.count == denominators.count && denominators.count == result.count)
            
            var n = Int32(numerators.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                numerators.withUnsafeBufferPointer { num in
                    denominators.withUnsafeBufferPointer { denom in
                        vvfmod(dest.baseAddress!,
                               num.baseAddress!,
                               denom.baseAddress!,
                               &n)
                    }
                }
            }
    }
    
    // MARK: Remainder
    
    /// Calculates the remainder after dividing each element in `numerators` by the corresponding element in `denominators`, single-precision.
    ///
    /// - Parameter numerators: Input numerators.
    /// - Parameter denominators: Input denominators.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func remainder<T, U, V>(numerators: T,
                                   denominators: U,
                                   result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        T.Element == Float, U.Element == Float, V.Element == Float {
            
            precondition(denominators.count == denominators.count && denominators.count == result.count)
            
            var n = Int32(numerators.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                numerators.withUnsafeBufferPointer { num in
                    denominators.withUnsafeBufferPointer { denom in
                        vvremainderf(dest.baseAddress!,
                                     num.baseAddress!,
                                     denom.baseAddress!,
                                     &n)
                    }
                }
            }
    }
    
    /// Calculates the remainder after dividing each element in `numerators` by the corresponding element in `denominators`, double-precision.
    ///
    /// - Parameter numerators: Input numerators.
    /// - Parameter denominators: Input denominators.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func remainder<T, U, V>(numerators: T,
                                   denominators: U,
                                   result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        T.Element == Double, U.Element == Double, V.Element == Double {
            
            precondition(denominators.count == denominators.count && denominators.count == result.count)
            
            var n = Int32(numerators.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                numerators.withUnsafeBufferPointer { num in
                    denominators.withUnsafeBufferPointer { denom in
                        vvremainder(dest.baseAddress!,
                                    num.baseAddress!,
                                    denom.baseAddress!,
                                    &n)
                    }
                }
            }
    }
    
    // MARK: Integer Truncation
    
    /// Calculates the integer truncation of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func integerTruncation<U, V>(_ vector: U,
                                        result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the integer truncation of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func integerTruncation<U, V>(_ vector: U,
                                        result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the nearest integer to each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func nearestInteger<U, V>(_ vector: U,
                                     result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the nearest integer to each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func nearestInteger<U, V>(_ vector: U,
                                     result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the reciprocal square root of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func reciprocalSquareRoot<U, V>(_ vector: U,
                                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the reciprocal square root of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func reciprocalSquareRoot<U, V>(_ vector: U,
                                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the square root of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func squareRoot<U, V>(_ vector: U,
                                 result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the square root of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func squareRoot<U, V>(_ vector: U,
                                 result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the reciprocal of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func reciprocal<U, V>(_ vector: U,
                                 result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the reciprocal of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func reciprocal<U, V>(_ vector: U,
                                 result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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

extension vDSP {
    
    // MARK: Exponential
    
    public enum ExponentialType {
        case exp    // eˣ
        case expm1  // eˣ˗1
        case exp2   // 2ˣ
    }
    
    /// Calculates an exponential function of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter exponentialType: Specifies the function: e raised to the power of each element, e raised to the power of each element minus one, or 2 raised to the power of each element.
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func exponential<U, V>(_ vector: U,
                                  exponentialType: ExponentialType,
                                  result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    
                    switch exponentialType {
                    case .exp:
                        vvexpf(dest.baseAddress!,
                               src.baseAddress!,
                               &n)
                    case .expm1:
                        vvexpm1f(dest.baseAddress!,
                                 src.baseAddress!,
                                 &n)
                    case .exp2:
                        vvexp2f(dest.baseAddress!,
                                src.baseAddress!,
                                &n)
                    }
                }
            }
    }
    
    /// Calculates an exponential function of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter exponentialType: Specifies the function: e raised to the power of each element, e raised to the power of each element minus one, or 2 raised to the power of each element.
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func exponential<U, V>(_ vector: U,
                                  exponentialType: ExponentialType,
                                  result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    
                    switch exponentialType {
                    case .exp:
                        vvexp(dest.baseAddress!,
                              src.baseAddress!,
                              &n)
                    case .expm1:
                        vvexpm1(dest.baseAddress!,
                                src.baseAddress!,
                                &n)
                    case .exp2:
                        vvexp2(dest.baseAddress!,
                               src.baseAddress!,
                               &n)
                    }
                }
            }
    }
    
    // MARK: Logarithm
    
    public enum LogarithmBase {
        case two
        case ten
    }
    
    /// Calculates either the base two or base ten logarithm of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter base: Specifies the logarithm base.
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func logarithm<U, V>(_ vector: U,
                                base: LogarithmBase,
                                result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        U.Element == Float, V.Element == Float {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    
                    switch base {
                    case .two:
                        vvlog2f(dest.baseAddress!,
                                src.baseAddress!,
                                &n)
                    case .ten:
                        vvlog10f(dest.baseAddress!,
                                 src.baseAddress!,
                                 &n)
                    }
                }
            }
    }
    
    /// Calculates either the base two or base ten logarithm of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter base: Specifies the logarithm base.
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func logarithm<U, V>(_ vector: U,
                                base: LogarithmBase,
                                result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
        U.Element == Double, V.Element == Double {
            
            precondition(vector.count == result.count)
            
            var n = Int32(vector.count)
            
            result.withUnsafeMutableBufferPointer { dest in
                vector.withUnsafeBufferPointer { src in
                    
                    switch base {
                    case .two:
                        vvlog2(dest.baseAddress!,
                               src.baseAddress!,
                               &n)
                    case .ten:
                        vvlog10(dest.baseAddress!,
                                src.baseAddress!,
                                &n)
                    }
                }
            }
    }
    
    // MARK: Unbiased Exponent
    
    /// Calculates the unbiased exponent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func unbiasedExponent<U, V>(_ vector: U,
                                       result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the unbiased exponent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter _ vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func unbiasedExponent<U, V>(_ vector: U,
                                       result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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

extension vDSP {
    
    // MARK: Power
    
    /// Calculates each element in `bases` rasied to the power of the corresponding element in `exponents`, writing the result to `result`, single-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func power<T, U, V>(bases: T,
                               exponents: U,
                               result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates each element in `bases` rasied to the power of the corresponding element in `exponents`, writing the result to `result`, double-precision.
    ///
    /// - Parameter bases: Input base values.
    /// - Parameter exponents: Input exponents.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func power<T, U, V>(bases: T,
                               exponents: U,
                               result: inout V)
        where
        T : _ContiguousCollection,
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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

extension vDSP {
    
    // MARK: Sine
    
    /// Calculates the sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sin<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sin<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the sine of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the sine of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cos<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cos<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the cosine of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cosPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the cosine of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cosPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinCos<T, U, V>(_ vector: T,
                                sinResult: inout U,
                                cosResult: inout V)
        where
        T : _ContiguousCollection,
        U : _MutableContiguousCollection,
        V : _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinCos<T, U, V>(_ vector: T,
                                sinResult: inout U,
                                cosResult: inout V)
        where
        T : _ContiguousCollection,
        U : _MutableContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tan<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tan<U, V>(_ vector: U,
                          result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the tangent of pi multiplied by each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tanPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the tangent of pi multiplied by each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tanPi<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arcsine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arcsin<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arcsine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arcsin<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arccosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arccos<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arccosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arccos<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arctangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arctan<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the arctangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func arctan<U, V>(_ vector: U,
                             result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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

extension vDSP {
    
    // MARK: Hyperbolic Sine
    
    /// Calculates the hyperbolic sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the hyperbolic sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func sinh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the hyperbolic cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cosh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the hyperbolic cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func cosh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the hyperbolic tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tanh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the hyperbolic tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func tanh<U, V>(_ vector: U,
                           result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic sine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func asinh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic sine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func asinh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic cosine of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func acosh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic cosine of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func acosh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic tangent of each element in `vector`, writing the result to `result`, single-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func atanh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
    
    /// Calculates the inverse hyperbolic tangent of each element in `vector`, writing the result to `result`, double-precision.
    ///
    /// - Parameter vector: Input values.
    /// - Parameter result: Output values.
    @inline(__always)
    @available(iOS 9999, macOS 9999, tvOS 9999, watchOS 9999, *)
    public static func atanh<U, V>(_ vector: U,
                            result: inout V)
        where
        U : _ContiguousCollection,
        V : _MutableContiguousCollection,
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
