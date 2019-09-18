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
    
    // MARK: Integer to floating-point conversion
    
    /// Converts an array of unsigned 8-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt8,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu8(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Converts an array of unsigned 8-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt8,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu8D(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    /// Converts an array of unsigned 16-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt16,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu16(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    /// Converts an array of unsigned 16-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt16,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu16D(src.baseAddress!, 1,
                                  dest.baseAddress!, 1,
                                  n)
                }
            }
    }
    
    /// Converts an array of unsigned 32-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt32,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu32(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    /// Converts an array of unsigned 32-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == UInt32,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vfltu32D(src.baseAddress!, 1,
                                  dest.baseAddress!, 1,
                                  n)
                }
            }
    }
    
    /// Converts an array of signed 8-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int8,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt8(src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Converts an array of signed 8-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int8,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt8D(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Converts an array of signed 16-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int16,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt16(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Converts an array of signed 16-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int16,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt16D(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    /// Converts an array of signed 32-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int32,
        V.Element == Float {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt32(src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                n)
                }
            }
    }
    
    /// Converts an array of signed 32-bit integers to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Int32,
        V.Element == Double {
            
            precondition(source.count == destination.count)
            
            let n = vDSP_Length(source.count)
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vflt32D(src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 n)
                }
            }
    }
    
    // MARK: Floating-point to integer conversion
    public enum RoundingMode {
        case towardZero
        case towardNearestInteger
    }
    
    /// Converts an array of single-precision floating-point values to signed 32-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Int32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix32(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardNearestInteger:
                        vDSP_vfixr32(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to signed 32-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Int32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix32D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardNearestInteger:
                        vDSP_vfixr32D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Converts an array of single-precision floating-point values to unsigned 16-bit integer values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == UInt16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu16(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardNearestInteger:
                        vDSP_vfixru16(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to unsigned 16-bit integer values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == UInt16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu16D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    case .towardNearestInteger:
                        vDSP_vfixru16D(src.baseAddress!, 1,
                                       dest.baseAddress!, 1,
                                       n)
                    }
                }
            }
    }
    
    /// Converts an array of single-precision floating-point values to unsigned 32-bit integer values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == UInt32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu32(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardNearestInteger:
                        vDSP_vfixru32(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to unsigned 32-bit integer values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == UInt32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu32D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    case .towardNearestInteger:
                        vDSP_vfixru32D(src.baseAddress!, 1,
                                       dest.baseAddress!, 1,
                                       n)
                    }
                }
            }
    }
    
    
    /// Converts an array of single-precision floating-point values to signed 16-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Int16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix16(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardNearestInteger:
                        vDSP_vfixr16(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to signed 16-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Int16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix16D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardNearestInteger:
                        vDSP_vfixr16D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Converts an array of single-precision floating-point values to signed 8-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Int8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix8(src.baseAddress!, 1,
                                   dest.baseAddress!, 1,
                                   n)
                    case .towardNearestInteger:
                        vDSP_vfixr8(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to signed 8-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Int8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfix8D(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardNearestInteger:
                        vDSP_vfixr8D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Converts an array of single-precision floating-point values to unsigned 8-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == UInt8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu8(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardNearestInteger:
                        vDSP_vfixru8(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    }
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to unsigned 8-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V,
                                             rounding: RoundingMode)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == UInt8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardZero:
                        vDSP_vfixu8D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardNearestInteger:
                        vDSP_vfixru8D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    }
                }
            }
    }
    
    /// Converts an array of single-precision floating-point values to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Double {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vspdp(src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
    
    /// Converts an array of double-precision floating-point values to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inlinable
    public static func convertElements<U, V>(of source: U,
                                             to destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double,
        V.Element == Float {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_vdpsp(src.baseAddress!, 1,
                               dest.baseAddress!, 1,
                               n)
                }
            }
    }
}

//===----------------------------------------------------------------------===//
//
//  Conversion functions that return the result
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_IntegerConvertable {}
extension UInt8: vDSP_IntegerConvertable {}
extension UInt16: vDSP_IntegerConvertable {}
extension UInt32: vDSP_IntegerConvertable {}
extension Int8: vDSP_IntegerConvertable {}
extension Int16: vDSP_IntegerConvertable {}
extension Int32: vDSP_IntegerConvertable {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FloatingPointConvertable {}
extension Float: vDSP_FloatingPointConvertable {}
extension Double: vDSP_FloatingPointConvertable {}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    // MARK: Integer to floating-point conversion
    
    /// Converts an array of unsigned 8-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == UInt8,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of unsigned 16-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == UInt16,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of unsigned 32-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == UInt32,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of signed 8-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == Int8,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of signed 16-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == Int16,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of signed 32-bit integers to floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func integerToFloatingPoint<T, U>(_ vector: T,
                                                    floatingPointType: U.Type) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == Int32,
        U: vDSP_FloatingPointConvertable {
            
            switch floatingPointType {
            case is Float.Type:
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Double.Type:
                let result = Array<Double>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
                
            default:
                fatalError("\(floatingPointType) not supported as a destination type.")
            }
    }
    
    // MARK: Floating-point to integer conversion
    
    /// Converts an array of single-precision floating-point values to integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func floatingPointToInteger<T, U>(_ vector: T,
                                                    integerType: U.Type,
                                                    rounding: RoundingMode) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == Float,
        U: vDSP_IntegerConvertable {
            
            switch integerType {
            case is UInt8.Type:
                let result = Array<UInt8>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is UInt16.Type:
                let result = Array<UInt16>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is UInt32.Type:
                let result = Array<UInt32>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int8.Type:
                let result = Array<Int8>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int16.Type:
                let result = Array<Int16>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int32.Type:
                let result = Array<Int32>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            default:
                fatalError("\(integerType) not supported as a destination type.")
            }
    }
    
    /// Converts an array of double-precision floating-point values to integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func floatingPointToInteger<T, U>(_ vector: T,
                                                    integerType: U.Type,
                                                    rounding: RoundingMode) -> [U]
        where
        T: AccelerateBuffer,
        T.Element == Double,
        U: vDSP_IntegerConvertable {
            
            switch integerType {
            case is UInt8.Type:
                let result = Array<UInt8>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is UInt16.Type:
                let result = Array<UInt16>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is UInt32.Type:
                let result = Array<UInt32>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int8.Type:
                let result = Array<Int8>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int16.Type:
                let result = Array<Int16>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            case is Int32.Type:
                let result = Array<Int32>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    convertElements(of: vector,
                                    to: &buffer,
                                    rounding: rounding)
                    
                    initializedCount = vector.count
                }
                
                return result as! [U]
            default:
                fatalError("\(integerType) not supported as a destination type.")
            }
    }
    
    // MARK: Floating-point to floating-point conversion
    
    /// Converts an array of single-precision floating-point values to double-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func floatToDouble<U>(_ source: U) -> [Double]
        where
        U: AccelerateBuffer,
        U.Element == Float {
            let result = Array<Double>(unsafeUninitializedCapacity: source.count) {
                buffer, initializedCount in
                
                convertElements(of: source,
                                to: &buffer)
                
                initializedCount = source.count
            }
            
            return result
    }
    
    /// Converts an array of double-precision floating-point values to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Returns: Conversion result.
    @inlinable
    public static func doubleToFloat<U>(_ source: U) -> [Float]
        where
        U: AccelerateBuffer,
        U.Element == Double {
            let result = Array<Float>(unsafeUninitializedCapacity: source.count) {
                buffer, initializedCount in
                
                convertElements(of: source,
                                to: &buffer)
                
                initializedCount = source.count
            }
            
            return result
    }
}
