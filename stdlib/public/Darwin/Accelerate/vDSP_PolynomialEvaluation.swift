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
    
    /// Evaluates a polynomial using specified coefficients and independent variables. Single-precision.
    ///
    /// - Parameter coefficients: Coefficients.
    /// - Parameter variables: Independent variables.
    /// - Returns: The polynomial evaluation result.
    ///
    /// For example, given the coefficients `[10, 20, 30]`, and independent variables `[2, 5]`,
    /// the result is calculated as:
    ///
    /// `result[0] = (10 * 2²) + (20 * 2¹) + (30 * 2⁰) // 110`
    ///
    /// `result[1] = (10 * 5²) + (20 * 5¹) + (30 * 5⁰) // 380`
    @inlinable
    public static func evaluatePolynomial<U>(usingCoefficients coefficients: [Float],
                                             withVariables variables: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: variables.count) {
                buffer, initializedCount in
                
                evaluatePolynomial(usingCoefficients: coefficients,
                                   withVariables: variables,
                                   result: &buffer)
                
                initializedCount = variables.count
            }
            
            return result
    }
    
    /// Evaluates a polynomial using specified coefficients and independent variables. Single-precision.
    ///
    /// - Parameter coefficients: Coefficients.
    /// - Parameter variables: Independent variables.
    /// - Parameter result: Destination vector.
    ///
    /// For example, given the coefficients `[10, 20, 30]`, and independent variables `[2, 5]`,
    /// the result is calculated as:
    ///
    /// `result[0] = (10 * 2²) + (20 * 2¹) + (30 * 2⁰) // 110`
    ///
    /// `result[1] = (10 * 5²) + (20 * 5¹) + (30 * 5⁰) // 380`
    @inlinable
    public static func evaluatePolynomial<U, V>(usingCoefficients coefficients: [Float],
                                                withVariables variables: U,
                                                result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = vDSP_Length(min(variables.count,
                                    result.count))
            
            let degreeOfPolynomial = vDSP_Length(coefficients.count - 1)
            
            result.withUnsafeMutableBufferPointer { dest in
                variables.withUnsafeBufferPointer { src in
                    vDSP_vpoly(coefficients, 1,
                               src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n,
                               degreeOfPolynomial)
                }
            }
    }
    
    /// Evaluates a polynomial using specified coefficients and independent variables. Double-precision.
    ///
    /// - Parameter coefficients: Coefficients.
    /// - Parameter variables: Independent variables.
    /// - Returns: The polynomial evaluation result.
    ///
    /// For example, given the coefficients `[10, 20, 30]`, and independent variables `[2, 5]`,
    /// the result is calculated as:
    ///
    /// `result[0] = (10 * 2²) + (20 * 2¹) + (30 * 2⁰) // 110`
    ///
    /// `result[1] = (10 * 5²) + (20 * 5¹) + (30 * 5⁰) // 380`
    @inlinable
    public static func evaluatePolynomial<U>(usingCoefficients coefficients: [Double],
                                             withVariables variables: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: variables.count) {
                buffer, initializedCount in
                
                evaluatePolynomial(usingCoefficients: coefficients,
                                   withVariables: variables,
                                   result: &buffer)
                
                initializedCount = variables.count
            }
            
            return result
    }
    
    /// Evaluates a polynomial using specified coefficients and independent variables. Double-precision.
    ///
    /// - Parameter coefficients: Coefficients.
    /// - Parameter variables: Independent variables.
    /// - Parameter result: Destination vector.
    ///
    /// For example, given the coefficients `[10, 20, 30]`, and independent variables `[2, 5]`,
    /// the result is calculated as:
    ///
    /// `result[0] = (10 * 2²) + (20 * 2¹) + (30 * 2⁰) // 110`
    ///
    /// `result[1] = (10 * 5²) + (20 * 5¹) + (30 * 5⁰) // 380`
    @inlinable
    public static func evaluatePolynomial<U, V>(usingCoefficients coefficients: [Double],
                                                withVariables variables: U,
                                                result: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = vDSP_Length(min(variables.count,
                                    result.count))
            
            let degreeOfPolynomial = vDSP_Length(coefficients.count - 1)
            
            result.withUnsafeMutableBufferPointer { dest in
                variables.withUnsafeBufferPointer { src in
                    vDSP_vpolyD(coefficients, 1,
                                src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n,
                                degreeOfPolynomial)
                }
            }
    }
    
}
