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
    
    /// Rectangular to polar conversion, single-precision.
    ///
    /// - Parameter rectangularCoordinates: Source vector, represented as consecutive x, y pairs.
    /// - Returns: Polar coordinates, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    @inlinable
    public static func rectangularToPolar<U>(_ rectangularCoordinates: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: rectangularCoordinates.count) {
                buffer, initializedCount in
                
                convert(rectangularCoordinates: rectangularCoordinates,
                        toPolarCoordinates: &buffer)
                
                initializedCount = rectangularCoordinates.count
            }
            
            return result
    }
    
    /// Rectangular to polar conversion, single-precision.
    ///
    /// - Parameter rectangularCoordinates: Source vector, represented as consecutive x, y pairs.
    /// - Parameter polarCoordinates: Destination vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    @inlinable
    public static func convert<U, V>(rectangularCoordinates: U,
                                     toPolarCoordinates polarCoordinates: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Float {
            
            let n = rectangularCoordinates.count
            precondition(polarCoordinates.count == n)
            
            polarCoordinates.withUnsafeMutableBufferPointer { dest in
                rectangularCoordinates.withUnsafeBufferPointer { src in
                    vDSP_polar(src.baseAddress!, 2,
                               dest.baseAddress!, 2,
                               vDSP_Length(n / 2))
                }
            }
    }
    
    /// Rectangular to polar conversion, double-precision.
    ///
    /// - Parameter rectangularCoordinates: Source vector, represented as consecutive x, y pairs.
    /// - Returns: Polar coordinates, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    @inlinable
    public static func rectangularToPolar<U>(_ rectangularCoordinates: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: rectangularCoordinates.count) {
                buffer, initializedCount in
                
                convert(rectangularCoordinates: rectangularCoordinates,
                        toPolarCoordinates: &buffer)
                
                initializedCount = rectangularCoordinates.count
            }
            
            return result
    }
    
    /// Rectangular to polar conversion, double-precision.
    ///
    /// - Parameter rectangularCoordinates: Source vector, represented as consecutive x, y pairs.
    /// - Parameter polarCoordinates: Destination vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    @inlinable
    public static func convert<U, V>(rectangularCoordinates: U,
                                     toPolarCoordinates polarCoordinates: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Double {
            
            let n = rectangularCoordinates.count
            precondition(polarCoordinates.count == n)
            
            polarCoordinates.withUnsafeMutableBufferPointer { dest in
                rectangularCoordinates.withUnsafeBufferPointer { src in
                    vDSP_polarD(src.baseAddress!, 2,
                                dest.baseAddress!, 2,
                                vDSP_Length(n / 2))
                }
            }
    }
    
    /// Polar to rectangular conversion, single-precision.
    ///
    /// - Parameter polarCoordinates: Source vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    /// - Returns: Rectangular coordinates, represented as consecutive x, y pairs.
    @inlinable
    public static func polarToRectangular<U>(_ polarCoordinates: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: polarCoordinates.count) {
                buffer, initializedCount in
                
                convert(polarCoordinates: polarCoordinates,
                        toRectangularCoordinates: &buffer)
                
                initializedCount = polarCoordinates.count
            }
            
            return result
    }
    
    /// Polar to rectangular conversion, single-precision.
    ///
    /// - Parameter polarCoordinates: Source vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    /// - Parameter rectangularCoordinates: Destination vector, represented as consecutive x, y pairs.
    @inlinable
    public static func convert<U, V>(polarCoordinates: U,
                                     toRectangularCoordinates rectangularCoordinates: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Float {
            
            let n = rectangularCoordinates.count
            precondition(polarCoordinates.count == n)
            
            rectangularCoordinates.withUnsafeMutableBufferPointer { dest in
                polarCoordinates.withUnsafeBufferPointer { src in
                    vDSP_rect(src.baseAddress!, 2,
                              dest.baseAddress!, 2,
                              vDSP_Length(n / 2))
                }
            }
    }
    
    /// Polar to rectangular conversion, double-precision.
    ///
    /// - Parameter polarCoordinates: Source vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    /// - Returns: Rectangular coordinates, represented as consecutive x, y pairs.
    @inlinable
    public static func polarToRectangular<U>(_ polarCoordinates: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: polarCoordinates.count) {
                buffer, initializedCount in
                
                convert(polarCoordinates: polarCoordinates,
                        toRectangularCoordinates: &buffer)
                
                initializedCount = polarCoordinates.count
            }
            
            return result
    }
    
    /// Polar to rectangular conversion, double-precision.
    ///
    /// - Parameter polarCoordinates: Source vector, represented as consecutive rho, (radius) theta (angle in radians) pairs.
    /// - Parameter rectangularCoordinates: Destination vector, represented as consecutive x, y pairs.
    @inlinable
    public static func convert<U, V>(polarCoordinates: U,
                                     toRectangularCoordinates rectangularCoordinates: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Double {
            
            let n = rectangularCoordinates.count
            precondition(polarCoordinates.count == n)
            
            rectangularCoordinates.withUnsafeMutableBufferPointer { dest in
                polarCoordinates.withUnsafeBufferPointer { src in
                    vDSP_rectD(src.baseAddress!, 2,
                               dest.baseAddress!, 2,
                               vDSP_Length(n / 2))
                }
            }
    }
}
