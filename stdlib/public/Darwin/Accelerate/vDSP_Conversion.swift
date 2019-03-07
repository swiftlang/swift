//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Accelerate

// Top level enum for namespaced vDSP and vForce based functions
public enum vDSP {}


extension vDSP {

    // MARK: Integer to floating-point conversion
    
    /// Converts an array of unsigned 8-bit integers to single-precision floating-point values.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
        case towardsZero
        case towardsNearestInteger
    }
    
    /// Converts an array of single-precision floating-point values to signed 32-bit integer values using specified rounding.
    ///
    /// - Parameter source: Source vector.
    /// - Parameter destination: Destination vector.
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == Int32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix32(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == Int32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix32D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == UInt16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu16(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == UInt16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu16D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == UInt32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu32(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == UInt32 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu32D(src.baseAddress!, 1,
                                      dest.baseAddress!, 1,
                                      n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == Int16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix16(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == Int16 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix16D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == Int8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix8(src.baseAddress!, 1,
                                   dest.baseAddress!, 1,
                                   n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == Int8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfix8D(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Float,
        V.Element == UInt8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu8(src.baseAddress!, 1,
                                    dest.baseAddress!, 1,
                                    n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V,
                                     rounding: RoundingMode)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
        U.Element == Double,
        V.Element == UInt8 {
            let n = vDSP_Length(min(source.count,
                                    destination.count))
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    switch rounding {
                    case .towardsZero:
                        vDSP_vfixu8D(src.baseAddress!, 1,
                                     dest.baseAddress!, 1,
                                     n)
                    case .towardsNearestInteger:
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
    @inline(__always)
    @available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
    public static func convert<U, V>(_ source: U,
                                     to destination: inout V)
        where
        U: _ContiguousCollection,
        V: _MutableContiguousCollection,
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
