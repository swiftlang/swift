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
    
    /// Converts power to decibels, single-precision.
    ///
    /// - Parameter power: Source vector.
    /// - Parameter zeroReference: Zero reference.
    /// - Returns: `power` converted to decibels.
    @inlinable
    public static func powerToDecibels<U>(_ power: U,
                                          zeroReference: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: power.count) {
                buffer, initializedCount in
                
                convert(power: power,
                        toDecibels: &buffer,
                        zeroReference: zeroReference)
                
                initializedCount = power.count
            }
            
            return result
    }
    
    /// Converts power to decibels, single-precision.
    ///
    /// - Parameter power: Source vector.
    /// - Parameter decibels: Destination vector.
    /// - Parameter zeroReference: Zero reference.
    @inlinable
    public static func convert<U, V>(power: U,
                                     toDecibels decibels: inout V,
                                     zeroReference: Float)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = decibels.count
            precondition(power.count == n)
            
            decibels.withUnsafeMutableBufferPointer { db in
                power.withUnsafeBufferPointer { pwr in
                    withUnsafePointer(to: zeroReference) { zref in
                        vDSP_vdbcon(pwr.baseAddress!, 1,
                                    zref,
                                    db.baseAddress!, 1,
                                    vDSP_Length(n),
                                    0)
                    }
                }
            }
    }
    
    /// Converts power to decibels, double-precision.
    ///
    /// - Parameter power: Source vector.
    /// - Parameter zeroReference: Zero reference.
    /// - Returns: `power` converted to decibels.
    @inlinable
    public static func powerToDecibels<U>(_ power: U,
                                          zeroReference: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: power.count) {
                buffer, initializedCount in
                
                convert(power: power,
                        toDecibels: &buffer,
                        zeroReference: zeroReference)
                
                initializedCount = power.count
            }
            
            return result
    }
    
    /// Converts power to decibels, double-precision.
    ///
    /// - Parameter power: Source vector.
    /// - Parameter decibels: Destination vector.
    /// - Parameter zeroReference: Zero reference.
    @inlinable
    public static func convert<U, V>(power: U,
                                     toDecibels decibels: inout V,
                                     zeroReference: Double)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = decibels.count
            precondition(power.count == n)
            
            decibels.withUnsafeMutableBufferPointer { db in
                power.withUnsafeBufferPointer { pwr in
                    withUnsafePointer(to: zeroReference) { zref in
                        vDSP_vdbconD(pwr.baseAddress!, 1,
                                     zref,
                                     db.baseAddress!, 1,
                                     vDSP_Length(n),
                                     0)
                    }
                }
            }
    }
    
    /// Converts amplitude to decibels, single-precision.
    ///
    /// - Parameter amplitude: Source vector.
    /// - Parameter zeroReference: Zero reference.
    /// - Returns: `amplitude` converted to decibels.
    @inlinable
    public static func amplitudeToDecibels<U>(_ amplitude: U,
                                              zeroReference: Float) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            
            let result = Array<Float>(unsafeUninitializedCapacity: amplitude.count) {
                buffer, initializedCount in
                
                convert(amplitude: amplitude,
                        toDecibels: &buffer,
                        zeroReference: zeroReference)
                
                initializedCount = amplitude.count
            }
            
            return result
    }
    
    /// Converts amplitude to decibels, single-precision.
    ///
    /// - Parameter amplitude: Source vector.
    /// - Parameter decibels: Destination vector.
    /// - Parameter zeroReference: Zero reference.
    @inlinable
    public static func convert<U, V>(amplitude: U,
                                     toDecibels decibels: inout V,
                                     zeroReference: Float)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            let n = decibels.count
            precondition(amplitude.count == n)
            
            decibels.withUnsafeMutableBufferPointer { db in
                amplitude.withUnsafeBufferPointer { amp in
                    withUnsafePointer(to: zeroReference) { zref in
                        vDSP_vdbcon(amp.baseAddress!, 1,
                                    zref,
                                    db.baseAddress!, 1,
                                    vDSP_Length(n),
                                    1)
                    }
                }
            }
    }
    
    /// Converts amplitude to decibels, double-precision.
    ///
    /// - Parameter amplitude: Source vector.
    /// - Parameter zeroReference: Zero reference.
    /// - Returns: `amplitude` converted to decibels.
    @inlinable
    public static func amplitudeToDecibels<U>(_ amplitude: U,
                                              zeroReference: Double) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            
            let result = Array<Double>(unsafeUninitializedCapacity: amplitude.count) {
                buffer, initializedCount in
                
                convert(amplitude: amplitude,
                        toDecibels: &buffer,
                        zeroReference: zeroReference)
                
                initializedCount = amplitude.count
            }
            
            return result
    }
    
    /// Converts amplitude to decibels, double-precision.
    ///
    /// - Parameter amplitude: Source vector.
    /// - Parameter decibels: Destination vector.
    /// - Parameter zeroReference: Zero reference.
    @inlinable
    public static func convert<U, V>(amplitude: U,
                                     toDecibels decibels: inout V,
                                     zeroReference: Double)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            let n = decibels.count
            precondition(amplitude.count == n)
            
            decibels.withUnsafeMutableBufferPointer { db in
                amplitude.withUnsafeBufferPointer { amp in
                    withUnsafePointer(to: zeroReference) { zref in
                        vDSP_vdbconD(amp.baseAddress!, 1,
                                     zref,
                                     db.baseAddress!, 1,
                                     vDSP_Length(n),
                                     1)
                    }
                }
            }
    }
}
