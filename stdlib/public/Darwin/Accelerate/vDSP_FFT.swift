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
//  1D and 2D Fast Fourier Transform
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    
    /// An enumeration that defines the size of the FFT decomposition.
    public enum Radix {
        case radix2
        case radix3
        case radix5
        
        public var fftRadix: FFTRadix {
            switch self {
            case .radix2:
                return FFTRadix(kFFTRadix2)
            case .radix3:
                return FFTRadix(kFFTRadix3)
            case .radix5:
                return FFTRadix(kFFTRadix5)
            }
        }
    }
    
    /// A class that provides forward and inverse FFT on `DSPSplitComplex` or `DSPDoubleSplitComplex` structure.
    public class FFT<T: vDSP_FourierTransformable> {
        
        let log2n: vDSP_Length
        let radix: Radix
        
        fileprivate let fftSetup: OpaquePointer
        
        /// Initializes a new fast Fourier transform structure.
        ///
        /// - Parameter log2n: The base-two logarithm of the maximum number of elements to be transformed.
        /// - Parameter radix: Specifies radix options.
        public init?(log2n: vDSP_Length,
                     radix: Radix,
                     ofType: T.Type) {
            
            self.log2n = log2n
            self.radix = radix
            
            guard let setup = T.FFTFunctions.makeFFTSetup(log2n: log2n,
                                                          radix: radix) else {
                                                            return nil
            }
            
            fftSetup = setup
        }
        
        /// Computes an out-of-place single-precision real forward or inverse fast Fourier transform.
        ///
        /// - Parameter input: Complex input vector.
        /// - Parameter output: Complex output vector.
        /// - Parameter direction: The transform direction.
        public func transform<T: vDSP_FourierTransformable>(input: T,
                                                            output: inout T,
                                                            direction: vDSP.FourierTransformDirection) {
            
            vDSP_FFTFunctions.fftTransform(fftSetup: fftSetup,
                                           log2n: log2n,
                                           source: input,
                                           destination: &output,
                                           direction: direction)
        }
        
        /// Computes an out-of-place single-precision real forward fast Fourier transform.
        ///
        /// - Parameter input: Complex input vector.
        /// - Parameter output: Complex output vector.
        public func forward(input: DSPSplitComplex,
                            output: inout DSPSplitComplex) {
            transform(input: input,
                      output: &output,
                      direction: .forward)
        }
        
        /// Computes an out-of-place single-precision real inverse fast Fourier transform.
        ///
        /// - Parameter input: Complex input vector.
        /// - Parameter output: Complex output vector.
        public func inverse(input: DSPSplitComplex,
                            output: inout DSPSplitComplex) {
            transform(input: input,
                      output: &output,
                      direction: .inverse)
        }
        
        /// Frees memory associated with this `FFT` struct.
        deinit {
            T.FFTFunctions.destroySetup(fftSetup)
        }
    }
    
    // MARK: 2D FFT
    
    /// A class that provides forward and inverse 2D FFT on `DSPSplitComplex` or `DSPDoubleSplitComplex` structure.
    public class FFT2D<T: vDSP_FourierTransformable>: FFT<T> {
        
        let width: Int
        let height: Int
        
        /// Initializes a new fast Fourier transform structure for 2D FFT.
        ///
        /// - Parameter width: The width of the matrix to be transformed.
        /// - Parameter height: The width of the matrix to be transformed.
        required public init?(width: Int,
                              height: Int,
                              ofType: T.Type) {
            self.width = width
            self.height = height
            
            let log2n = vDSP_Length(log2(Float(width * height)))
            
            super.init(log2n: log2n,
                       radix: .radix2,
                       ofType: ofType)
        }
        
        /// Computes an out-of-place 2D fast Fourier transform.
        ///
        /// - Parameter input: Complex input vector.
        /// - Parameter output: Complex output vector.
        /// - Parameter direction: Specifies transform direction.
        override public func transform<T: vDSP_FourierTransformable>(input: T,
                                                                     output: inout T,
                                                                     direction: vDSP.FourierTransformDirection) {
            vDSP_FFTFunctions.fftTransform2D(fftSetup: fftSetup,
                                             width: width,
                                             height: height,
                                             source: input,
                                             destination: &output,
                                             direction: direction)
        }
    }
    
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FourierTransformFunctions {
    associatedtype SplitComplex
    
    static func makeFFTSetup(log2n: vDSP_Length,
                             radix: vDSP.Radix) -> OpaquePointer?
    
    static func transform(fftSetup: OpaquePointer,
                          log2n: vDSP_Length,
                          source: UnsafePointer<SplitComplex>,
                          destination: UnsafeMutablePointer<SplitComplex>,
                          direction: vDSP.FourierTransformDirection)
    
    static func transform2D(fftSetup: OpaquePointer,
                            width: Int,
                            height: Int,
                            source: UnsafePointer<SplitComplex>,
                            destination: UnsafeMutablePointer<SplitComplex>,
                            direction: vDSP.FourierTransformDirection)
    
    static func destroySetup(_ setup: OpaquePointer)
}

//===----------------------------------------------------------------------===//
//
//  Type-specific FFT function implementations
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public struct vDSP_SplitComplexFloat: vDSP_FourierTransformFunctions {
    public typealias SplitComplex = DSPSplitComplex
    
    /// Returns a setup structure to perform a fast Fourier transform.
    public static func makeFFTSetup(log2n: vDSP_Length,
                                    radix: vDSP.Radix) -> OpaquePointer? {
        
        return vDSP_create_fftsetup(
            log2n,
            radix.fftRadix)
    }
    
    /// Performs a 1D fast Fourier transform.
    public static func transform(fftSetup: OpaquePointer,
                                 log2n: vDSP_Length,
                                 source: UnsafePointer<SplitComplex>,
                                 destination: UnsafeMutablePointer<SplitComplex>,
                                 direction: vDSP.FourierTransformDirection) {
        vDSP_fft_zrop(fftSetup,
                      source, 1,
                      destination, 1,
                      log2n,
                      direction.fftDirection)
    }
    
    /// Performs a 2D fast Fourier transform.
    public static func transform2D(fftSetup: OpaquePointer,
                                   width: Int,
                                   height: Int,
                                   source: UnsafePointer<SplitComplex>,
                                   destination: UnsafeMutablePointer<SplitComplex>,
                                   direction: vDSP.FourierTransformDirection) {
        vDSP_fft2d_zrop(fftSetup,
                        source, 1, 0,
                        destination, 1, 0,
                        vDSP_Length(log2(Float(width))),
                        vDSP_Length(log2(Float(height))),
                        direction.fftDirection)
    }
    
    /// Releases an FFT setup object.
    public static func destroySetup(_ setup: OpaquePointer) {
        vDSP_destroy_fftsetup(setup)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public struct vDSP_SplitComplexDouble: vDSP_FourierTransformFunctions {
    public typealias SplitComplex = DSPDoubleSplitComplex
    
    /// Returns a setup structure to perform a fast Fourier transform.
    public static func makeFFTSetup(log2n: vDSP_Length,
                                    radix: vDSP.Radix) -> OpaquePointer? {
        
        return vDSP_create_fftsetupD(
            log2n,
            radix.fftRadix)
    }
    
    /// Performs a 1D fast Fourier transform.
    public static func transform(fftSetup: OpaquePointer,
                                 log2n: vDSP_Length,
                                 source: UnsafePointer<SplitComplex>,
                                 destination: UnsafeMutablePointer<SplitComplex>,
                                 direction: vDSP.FourierTransformDirection) {
        vDSP_fft_zropD(fftSetup,
                       source, 1,
                       destination, 1,
                       log2n,
                       direction.fftDirection)
    }
    
    /// Performs a 2D fast Fourier transform.
    public static func transform2D(fftSetup: OpaquePointer,
                                   width: Int,
                                   height: Int,
                                   source: UnsafePointer<SplitComplex>,
                                   destination: UnsafeMutablePointer<SplitComplex>,
                                   direction: vDSP.FourierTransformDirection) {
        vDSP_fft2d_zropD(fftSetup,
                         source, 1, 0,
                         destination, 1, 0,
                         vDSP_Length(log2(Float(width))),
                         vDSP_Length(log2(Float(height))),
                         direction.fftDirection)
    }
    
    /// Releases an FFT setup object.
    public static func destroySetup(_ setup: OpaquePointer) {
        vDSP_destroy_fftsetupD(setup)
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FourierTransformable {
    associatedtype FFTFunctions: vDSP_FourierTransformFunctions where FFTFunctions.SplitComplex == Self
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DSPSplitComplex: vDSP_FourierTransformable {
    public typealias FFTFunctions = vDSP_SplitComplexFloat
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DSPDoubleSplitComplex: vDSP_FourierTransformable {
    public typealias FFTFunctions = vDSP_SplitComplexDouble
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
struct vDSP_FFTFunctions {
    /// Performs a 1D fast Fourier transform.
    @inlinable
    static func fftTransform<SplitComplex>(fftSetup: OpaquePointer,
                                           log2n: vDSP_Length,
                                           source: SplitComplex,
                                           destination: inout SplitComplex,
                                           direction: vDSP.FourierTransformDirection) where SplitComplex: vDSP_FourierTransformable {
        
        withUnsafePointer(to: source) { sourcePointer in
            SplitComplex.FFTFunctions.transform(fftSetup: fftSetup,
                                                log2n: log2n,
                                                source: sourcePointer,
                                                destination: &destination,
                                                direction: direction)
        }
    }
    
    /// Performs a 2D fast Fourier transform.
    @inlinable
    static func fftTransform2D<SplitComplex>(fftSetup: OpaquePointer,
                                             width: Int,
                                             height: Int,
                                             source: SplitComplex,
                                             destination: inout SplitComplex,
                                             direction: vDSP.FourierTransformDirection) where SplitComplex: vDSP_FourierTransformable {
        
        withUnsafePointer(to: source) { sourcePointer in
            SplitComplex.FFTFunctions.transform2D(fftSetup: fftSetup,
                                                  width: width,
                                                  height: height,
                                                  source: sourcePointer,
                                                  destination: &destination,
                                                  direction: direction)
        }
    }
}


@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DSPSplitComplex {
    
    /// Creates a new `DSPSplitComplex` structure from a real array not in even-odd split configuration.
    ///
    /// - Parameter inputArray: The source array of contiguous values.
    /// - Parameter realParts: An array of real parts of the complex numbers.
    /// - Parameter imaginaryParts: An array of imaginary parts of the complex numbers.
    public init(fromInputArray inputArray: [Float],
                realParts: inout [Float],
                imaginaryParts: inout [Float]) {
        
        self.init(realp: &realParts,
                  imagp: &imaginaryParts)
        
        inputArray.withUnsafeBytes{
            vDSP_ctoz([DSPComplex]($0.bindMemory(to: DSPComplex.self)), 2,
                      &self, 1,
                      vDSP_Length(inputArray.count / 2))
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension DSPDoubleSplitComplex {
    
    /// Creates a new `DSPDoubleSplitComplex` structure from a real array not in even-odd split configuration.
    ///
    /// - Parameter inputArray: The source array of contiguous values.
    /// - Parameter realParts: An array of real parts of the complex numbers.
    /// - Parameter imaginaryParts: An array of imaginary parts of the complex numbers.
    public init(fromInputArray inputArray: [Double],
                realParts: inout [Double],
                imaginaryParts: inout [Double]) {
        
        self.init(realp: &realParts,
                  imagp: &imaginaryParts)
        
        inputArray.withUnsafeBytes{
            vDSP_ctozD([DSPDoubleComplex]($0.bindMemory(to: DSPDoubleComplex.self)), 2,
                       &self, 1,
                       vDSP_Length(inputArray.count / 2))
        }
    }
}

extension Array where Element == Float {
    /// Creates a new array of single-precision values from a `DSPSplitComplex` structure.
    ///
    /// - Parameter scale: A multiplier to apply during conversion.
    /// - Parameter count: The length of the required resulting array (typically half the count of either the real or imaginary parts of the `DSPSplitComplex`.
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public init(fromSplitComplex splitComplex: DSPSplitComplex,
                scale: Float,
                count: Int) {
        var complexPairs = [DSPComplex](repeating: DSPComplex(real: 0, imag: 0),
                                        count: count / 2)
        
        withUnsafePointer(to: splitComplex) {
            vDSP_ztoc($0, 1,
                      &complexPairs, 2,
                      vDSP_Length(count / 2))
        }
        
        self = [Float](repeating: 0, count: count)
        
        complexPairs.withUnsafeBytes {
            guard let complexPairsUnsafePointer = $0.bindMemory(to: Float.self).baseAddress else {
                fatalError("Internal error")
            }
            
            vDSP_vsmul(complexPairsUnsafePointer, 1,
                       [scale],
                       &self, 1,
                       vDSP_Length(count))
        }
    }
}

extension Array where Element == Double {
    /// Creates a new array of single-precision values from a `DSPDoubleSplitComplex` structure.
    ///
    /// - Parameter scale: A multiplier to apply during conversion.
    /// - Parameter count: The length of the required resulting array (typically half the count of either the real or imaginary parts of the `DSPSplitComplex`.
    @available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
    public init(fromSplitComplex splitComplex: DSPDoubleSplitComplex,
                scale: Double,
                count: Int) {
        var complexPairs = [DSPDoubleComplex](repeating: DSPDoubleComplex(real: 0, imag: 0),
                                              count: count / 2)
        
        withUnsafePointer(to: splitComplex) {
            vDSP_ztocD($0, 1,
                       &complexPairs, 2,
                       vDSP_Length(count / 2))
        }
        
        self = [Double](repeating: 0, count: count)
        
        complexPairs.withUnsafeBytes {
            guard let complexPairsUnsafePointer = $0.bindMemory(to: Double.self).baseAddress else {
                fatalError("Internal error")
            }
            
            vDSP_vsmulD(complexPairsUnsafePointer, 1,
                        [scale],
                        &self, 1,
                        vDSP_Length(count))
        }
    }
}
