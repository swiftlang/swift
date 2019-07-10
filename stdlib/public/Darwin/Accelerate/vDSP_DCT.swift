///===----------------------------------------------------------------------===//
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
    
    /// An enum that specifies which DCT variant to perform.
    public enum DCTTransformType: CaseIterable {
        case II
        case III
        case IV
        
        public var dctType: vDSP_DCT_Type {
            switch self {
            case .II:
                return .II
            case .III:
                return .III
            case .IV:
                return .IV
            }
        }
    }
    
     /// A class that provides single-precision discrete cosine transform.
    public class DCT {
        
        fileprivate let dctSetup: vDSP_DFT_Setup
        
        /// Initializes a new discrete cosine transform instance.
        ///
        /// - Parameter previous: a previous vDSP_DCT instance to share data with.
        /// - Parameter count: the number of real elements to be transformed.
        /// - Parameter transformType: Specifies the transform type (type II for forward and type III for inverse).
        public init?(previous: DCT? = nil,
                     count: Int,
                     transformType: DCTTransformType) {
            
            guard let setup = vDSP.VectorizableFloat.makeDCTSetup(previous: previous,
                                                                  count: count,
                                                                  transformType: transformType) else {
                                                                    return nil
            }
            
            dctSetup = setup
        }
        
        /// Returns the single-precision real discrete cosine transform.
        ///
        /// - Parameter input: Real input vector.
        /// - Returns: Real output vector.
        public func transform<U>(_ vector: U) -> [Float]
            where
            U: AccelerateBuffer,
            U.Element == Float {
                
                let result = Array<Float>(unsafeUninitializedCapacity: vector.count) {
                    buffer, initializedCount in
                    
                    transform(vector,
                              result: &buffer)
                    
                    initializedCount = vector.count
                }
                
                return result
        }
        
        /// Computes an out-of-place single-precision real discrete cosine transform.
        ///
        /// - Parameter input: Real input vector.
        /// - Parameter output: Real output vector.
        public func transform<U, V>(_ vector: U, result: inout V)
            where
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element == Float, V.Element == Float {
                
                vDSP.VectorizableFloat.transform(dctSetup: dctSetup,
                                                 source: vector,
                                                 destination: &result)
        }
        
        deinit {
            vDSP_DFT_DestroySetup(dctSetup)
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
fileprivate protocol vDSP_FloatingPointDiscreteCosineTransformable: BinaryFloatingPoint {
    associatedtype DCTFunctions: vDSP_DCTFunctions where DCTFunctions.Scalar == Self
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Float: vDSP_FloatingPointDiscreteCosineTransformable {
    typealias DCTFunctions = vDSP.VectorizableFloat
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
fileprivate protocol vDSP_DCTFunctions {
    associatedtype Scalar
    
    static func makeDCTSetup(previous: vDSP.DCT?,
                             count: Int,
                             transformType: vDSP.DCTTransformType) -> OpaquePointer?
    
    static func transform<U, V>(dctSetup: OpaquePointer,
                                source: U,
                                destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Scalar, V.Element == Scalar
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP.VectorizableFloat: vDSP_DCTFunctions {
    
    fileprivate static func makeDCTSetup(previous: vDSP.DCT? = nil,
                                    count: Int,
                                    transformType: vDSP.DCTTransformType) -> OpaquePointer? {
        
        return vDSP_DCT_CreateSetup(previous?.dctSetup,
                                    vDSP_Length(count),
                                    transformType.dctType)
    }
    
    fileprivate static func transform<U, V>(dctSetup: OpaquePointer,
                                       source: U,
                                       destination: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float,
        V.Element == Float {
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_DCT_Execute(dctSetup,
                                     src.baseAddress!,
                                     dest.baseAddress!)
                }
            }
    }
}


