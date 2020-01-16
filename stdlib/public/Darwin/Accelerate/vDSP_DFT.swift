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

//===----------------------------------------------------------------------===//
//
//  Discrete Fourier Transform
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    /// An enumeration that specifies whether to perform complex-to-complex or
    /// complex-to-real discrete Fourier transform.
    // TODO: Should probably be @_frozen; check with Accelerate.
    public enum DFTTransformType {
        /// Specifies complex-to-complex discrete Fourier transform, forward
        /// or inverse
        case complexComplex
        
        /// Specifies real-to-complex (forward) or complex-to-real (inverse)
        /// discrete Fourier transform.
        case complexReal
    }
    
    /// A class that provides single- and double-precision discrete Fourier transform.
    public class DFT <T: vDSP_FloatingPointDiscreteFourierTransformable> {
        fileprivate let dftSetup: vDSP_DFT_Setup
        
        /// Initializes a new discrete Fourier transform structure.
        ///
        /// - Parameter previous: a previous vDSP_DFT instance to share data with.
        /// - Parameter count: the number of real elements to be transformed.
        /// - Parameter direction: Specifies the transform direction.
        /// - Parameter transformType: Specficies whether to forward transform is real-to-complex or complex-to-complex.
        public init?(previous: DFT? = nil,
                     count: Int,
                     direction: vDSP.FourierTransformDirection,
                     transformType: DFTTransformType,
                     ofType: T.Type) {
            
            guard let setup = T.DFTFunctions.makeDFTSetup(previous: previous,
                                                          count: count,
                                                          direction: direction,
                                                          transformType: transformType) else {
                                                            return nil
            }
            
            self.transformType = transformType
            
            dftSetup = setup
        }
        
        /// The transform type of this DFT.
        private let transformType: DFTTransformType
        
        /// Returns a single-precision real discrete Fourier transform.
        ///
        /// - Parameter inputReal: Input vector - real part.
        /// - Parameter inputImaginary: Input vector - imaginary part.
        /// - Returns: A tuple of two arrays representing the real and imaginary parts of the output.
        ///
        /// When the `transformType` is `complexComplex`, each input array (Ir, Ii)
        /// must have `count` elements, and the returned arrays have `count` elements.
        ///
        /// When the `transformType` is `complexReal`, each input array (Ir, Ii)
        /// must have `count` elements, and the returned arrays have `count / 2` elements.
        public func transform<U>(inputReal: U,
                                 inputImaginary: U) -> (real:[T], imaginary: [T])
            where
            U: AccelerateBuffer,
            U.Element == T {
                
                let n = transformType == .complexReal ? inputReal.count / 2 : inputReal.count
                
                var imaginaryResult: Array<T>!
                
                let realResult = Array<T>(unsafeUninitializedCapacity: n) {
                    realBuffer, realInitializedCount in
                    
                    imaginaryResult = Array<T>(unsafeUninitializedCapacity: n) {
                        imaginaryBuffer, imaginaryInitializedCount in
                        
                        transform(inputReal: inputReal,
                                  inputImaginary: inputImaginary,
                                  outputReal: &realBuffer,
                                  outputImaginary: &imaginaryBuffer)
                        
                        imaginaryInitializedCount = n
                    }
                    
                    realInitializedCount = n
                }
                
                return (real: realResult,
                        imaginary: imaginaryResult)
        }
        
        /// Computes an out-of-place single-precision real discrete Fourier transform.
        ///
        /// - Parameter inputReal: Input vector - real part.
        /// - Parameter inputImaginary: Input vector - imaginary part.
        /// - Parameter outputReal: Output vector - real part.
        /// - Parameter outputImaginary: Output vector - imaginary part.
        ///
        /// When the `transformType` is `complexComplex`, each array (Ir, Ii,
        /// Or, and Oi) must have `count` elements.
        ///
        /// When the `transformType` is `complexReal`, each array (Ir, Ii,
        /// Or, and Oi) must have `count/2` elements.
        public func transform<U, V>(inputReal: U,
                                    inputImaginary: U,
                                    outputReal: inout V,
                                    outputImaginary: inout V)
            where
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element == T, V.Element == T {
                
                T.DFTFunctions.transform(dftSetup: dftSetup,
                                         inputReal: inputReal,
                                         inputImaginary: inputImaginary,
                                         outputReal: &outputReal,
                                         outputImaginary: &outputImaginary)
        }
        
        deinit {
            T.DFTFunctions.destroySetup(dftSetup)
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FloatingPointDiscreteFourierTransformable: BinaryFloatingPoint {
    associatedtype DFTFunctions: vDSP_DFTFunctions where DFTFunctions.Scalar == Self
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Float: vDSP_FloatingPointDiscreteFourierTransformable {
    public typealias DFTFunctions = vDSP.VectorizableFloat
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Double: vDSP_FloatingPointDiscreteFourierTransformable {
    public typealias DFTFunctions = vDSP.VectorizableDouble
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_DFTFunctions {
    associatedtype Scalar
    
    /// Returns a setup structure to perform a discrete Fourier transform
    ///
    /// - Parameter previous: a previous vDSP_DFT instance to share data with.
    /// - Parameter count: the number of real elements to be transformed.
    /// - Parameter direction: Specifies the transform direction.
    /// - Parameter transformType: Specficies whether to forward transform is real-to-complex or complex-to-complex.
    static func makeDFTSetup<T>(previous: vDSP.DFT<T>?,
                                count: Int,
                                direction: vDSP.FourierTransformDirection,
                                transformType: vDSP.DFTTransformType) -> OpaquePointer?
    
    /// Computes an out-of-place single-precision real discrete Fourier transform.
    ///
    /// - Parameter dftSetup: A DCT setup object.
    /// - Parameter inputReal: Input vector - real part.
    /// - Parameter inputImaginary: Input vector - imaginary part.
    /// - Parameter outputReal: Output vector - real part.
    /// - Parameter outputImaginary: Output vector - imaginary part.
    static func transform<U, V>(dftSetup: OpaquePointer,
                                inputReal: U,
                                inputImaginary: U,
                                outputReal: inout V,
                                outputImaginary: inout V)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Scalar, V.Element == Scalar
    
    /// Releases a DFT setup object.
    static func destroySetup(_ setup: OpaquePointer)
}

//===----------------------------------------------------------------------===//
//
//  Type-specific DFT function implementations
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP.VectorizableFloat: vDSP_DFTFunctions {
    
    /// Returns a setup structure to perform a discrete Fourier transform
    ///
    /// - Parameter previous: a previous vDSP_DFT instance to share data with.
    /// - Parameter count: the number of real elements to be transformed.
    /// - Parameter direction: Specifies the transform direction.
    /// - Parameter transformType: Specficies whether to forward transform is real-to-complex or complex-to-complex.
    public static func makeDFTSetup<T>(previous: vDSP.DFT<T>? = nil,
                                       count: Int,
                                       direction: vDSP.FourierTransformDirection,
                                       transformType: vDSP.DFTTransformType) -> OpaquePointer?
        where T : vDSP_FloatingPointDiscreteFourierTransformable {
            
            switch transformType {
            case .complexComplex:
                return vDSP_DFT_zop_CreateSetup(previous?.dftSetup,
                                                vDSP_Length(count),
                                                direction.dftDirection)
            case .complexReal:
                return vDSP_DFT_zrop_CreateSetup(previous?.dftSetup,
                                                 vDSP_Length(count),
                                                 direction.dftDirection)
            }
    }
    
    /// Computes an out-of-place single-precision real discrete Fourier transform.
    ///
    /// - Parameter dftSetup: A DCT setup object.
    /// - Parameter inputReal: Input vector - real part.
    /// - Parameter inputImaginary: Input vector - imaginary part.
    /// - Parameter outputReal: Output vector - real part.
    /// - Parameter outputImaginary: Output vector - imaginary part.
    public static func transform<U, V>(dftSetup: OpaquePointer,
                                       inputReal: U,
                                       inputImaginary: U,
                                       outputReal: inout V,
                                       outputImaginary: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            inputReal.withUnsafeBufferPointer { Ir in
                inputImaginary.withUnsafeBufferPointer { Ii in
                    outputReal.withUnsafeMutableBufferPointer { Or in
                        outputImaginary.withUnsafeMutableBufferPointer { Oi in
                            
                            vDSP_DFT_Execute(dftSetup,
                                             Ir.baseAddress!,
                                             Ii.baseAddress!,
                                             Or.baseAddress!,
                                             Oi.baseAddress!)
                        }
                    }
                }
            }
    }
    
    /// Releases a DFT setup object.
    public static func destroySetup(_ setup: OpaquePointer) {
        vDSP_DFT_DestroySetup(setup)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP.VectorizableDouble: vDSP_DFTFunctions {
    
    /// Returns a data structure for use with to perform a discrete Fourier transform
    ///
    /// - Parameter previous: a previous vDSP_DFT instance to share data with.
    /// - Parameter count: the number of real elements to be transformed.
    /// - Parameter direction: Specifies the transform direction.
    /// - Parameter transformType: Specficies whether to forward transform is real-to-complex or complex-to-complex.
    public static func makeDFTSetup<T>(previous: vDSP.DFT<T>? = nil,
                                       count: Int,
                                       direction: vDSP.FourierTransformDirection,
                                       transformType: vDSP.DFTTransformType) -> OpaquePointer?
        where T : vDSP_FloatingPointDiscreteFourierTransformable {
            
            switch transformType {
            case .complexComplex:
                return vDSP_DFT_zop_CreateSetupD(previous?.dftSetup,
                                                 vDSP_Length(count),
                                                 direction.dftDirection)
            case .complexReal:
                return vDSP_DFT_zrop_CreateSetupD(previous?.dftSetup,
                                                  vDSP_Length(count),
                                                  direction.dftDirection)
            }
    }
    
    /// Computes an out-of-place single-precision real discrete Fourier transform.
    ///
    /// - Parameter dftSetup: A DCT setup object.
    /// - Parameter inputReal: Input vector - real part.
    /// - Parameter inputImaginary: Input vector - imaginary part.
    /// - Parameter outputReal: Output vector - real part.
    /// - Parameter outputImaginary: Output vector - imaginary part.
    public static func transform<U, V>(dftSetup: OpaquePointer,
                                       inputReal: U, inputImaginary: U,
                                       outputReal: inout V, outputImaginary: inout V)
        where
        U : AccelerateBuffer,
        V : AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            inputReal.withUnsafeBufferPointer { Ir in
                inputImaginary.withUnsafeBufferPointer { Ii in
                    outputReal.withUnsafeMutableBufferPointer { Or in
                        outputImaginary.withUnsafeMutableBufferPointer { Oi in
                            
                            vDSP_DFT_ExecuteD(dftSetup,
                                              Ir.baseAddress!,
                                              Ii.baseAddress!,
                                              Or.baseAddress!,
                                              Oi.baseAddress!)
                        }
                    }
                }
            }
    }
    
    /// Releases a DFT setup object.
    public static func destroySetup(_ setup: OpaquePointer) {
        vDSP_DFT_DestroySetupD(setup)
    }
}

