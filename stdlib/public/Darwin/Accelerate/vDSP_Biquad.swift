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
//  Biquad Structure
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP {
    public struct Biquad <T: vDSP_FloatingPointBiquadFilterable> {
        
        private var biquadRef: BiquadRef<T>
        
        /// Initializes a new single or multichannel cascaded biquad IIR structure.
        ///
        /// - Parameter coefficients: Array of double-precision real coefficients. Its length should be 5 times the number of sections in the biquad filter.
        /// - Parameter sectionCount: The number of sections in the biquad filter. The same number of sections is used for each channel, so only one value is specified.
        /// - Parameter channelCount: The number of input/output channels.
        /// - Parameter ofType: Specifies single- or double-precision.
        public init?(coefficients: [Double],
                     channelCount: vDSP_Length,
                     sectionCount: vDSP_Length,
                     ofType: T.Type) {
            
            if coefficients.count != 5 * channelCount * sectionCount {
                return nil
            }
            
            guard let ref = BiquadRef(coefficients: coefficients,
                                      channelCount: channelCount,
                                      sectionCount: sectionCount,
                                      ofType: ofType) else {
                                        return nil
            }
            
            biquadRef = ref
        }
        
        /// Applies a single- or double-precision single or multichannel biquad IIR filter, returning the filtered signal.
        public mutating func apply<U>(input: U) -> [T]
            where
            U: AccelerateBuffer,
            U.Element == T {
                
                let result = Array<T>(unsafeUninitializedCapacity: input.count) {
                    buffer, initializedCount in
                    
                    apply(input: input,
                          output: &buffer)
                    
                    initializedCount = input.count
                }
                
                return result
        }
        
        /// Applies a single- or double-precision single or multichannel biquad IIR filter, overwriting the supplied output.
        public mutating func apply<U, V>(input: U, output: inout V)
            where
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element == T, V.Element == T {
                
                // `apply(input:output:)` mutates `delays` and `setup`.
                if !isKnownUniquelyReferenced(&biquadRef) {
                    biquadRef = BiquadRef(coefficients: biquadRef.coefficients,
                                          channelCount: biquadRef.channelCount,
                                          sectionCount: biquadRef.sectionCount,
                                          ofType: T.self)!
                }
                
                biquadRef.apply(input: input,
                                output: &output)
        }
        
    }
    
    private class BiquadRef<T: vDSP_FloatingPointBiquadFilterable> {
        
        let coefficients: [Double]
        let channelCount: vDSP_Length
        let sectionCount: vDSP_Length
        var delays: [T]
        
        private let biquadSetup: OpaquePointer
        
        /// Initializes a new single or multichannel cascaded biquad IIR instance.
        ///
        /// - Parameter coefficients: Array of double-precision real coefficients. Its length should be 5 times the number of sections in the biquad filter.
        /// - Parameter sectionCount: The number of sections in the biquad filter. The same number of sections is used for each channel, so only one value is specified.
        /// - Parameter channelCount: The number of input/output channels.
        /// - Parameter ofType: Specifies single- or double-precision.
        ///
        /// For multiple channel signals, this class supports data in non-interleaved format. For example, when applying
        /// a filter to a signal represented by an array of 100 elements, elements 0...49 contain the data for channel 0,
        /// and elements 50...99 contain the data for channel 1.
        
        init?(coefficients: [Double],
              channelCount: vDSP_Length,
              sectionCount: vDSP_Length,
              ofType: T.Type) {
            
            self.coefficients = coefficients
            self.channelCount = channelCount
            self.sectionCount = sectionCount
            
            delays = [T](repeating: 0.0,
                         count: Int(2 * sectionCount) + 2)
            
            guard let setup = T.BiquadFunctions.makeBiquadSetup(channelCount: channelCount,
                                                                coefficients: coefficients,
                                                                sectionCount: sectionCount) else {
                                                                    return nil
            }
            
            biquadSetup = setup
        }
        
        /// Applies a single- or double-precision single or multichannel biquad IIR filter, overwriting the supplied output.
        func apply<U, V>(input: U, output: inout V)
            where
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element == T, V.Element == T {
                
                let n = vDSP_Length(min(input.count, output.count))
                
                if channelCount == 1 {
                    BiquadFunctions.applyBiquadSingle(source: input,
                                                      destination: &output,
                                                      delays: &delays,
                                                      setup: biquadSetup,
                                                      sectionCount: sectionCount,
                                                      count: n)
                } else  {
                    BiquadFunctions.applyBiquadMulti(source: input,
                                                     destination: &output,
                                                     setup: biquadSetup,
                                                     channelCount: channelCount,
                                                     count: n)
                }
        }
        
        deinit {
            BiquadFunctions.destroySetup(ofType: T.self,
                                         channelCount: channelCount,
                                         biquadSetup: biquadSetup)
        }
    }
    
    struct BiquadFunctions {
        
        @inlinable
        static func applyBiquadSingle<U, V, Scalar>(source: U,
                                                    destination: inout V,
                                                    delays: inout [Scalar],
                                                    setup: OpaquePointer,
                                                    sectionCount: UInt,
                                                    count: UInt)
            where
            Scalar: vDSP_FloatingPointBiquadFilterable,
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element == Scalar,
            V.Element == Scalar {
                
                Scalar.BiquadFunctions.applySingle(source: source,
                                                   destination: &destination,
                                                   delays: &delays,
                                                   setup: setup,
                                                   sectionCount: sectionCount,
                                                   count: count)
        }
        
        @inlinable
        static func applyBiquadMulti<U, V>(source: U,
                                           destination: inout V,
                                           setup: OpaquePointer,
                                           channelCount: UInt,
                                           count: UInt)
            where
            U: AccelerateBuffer,
            V: AccelerateMutableBuffer,
            U.Element: vDSP_FloatingPointBiquadFilterable,
            V.Element: vDSP_FloatingPointBiquadFilterable {
                
                source.withUnsafeBufferPointer { biquadInput in
                    let inputPointer = biquadInput.baseAddress!
                    destination.withUnsafeMutableBufferPointer { page in
                        let outputPointer = page.baseAddress!
                        
                        let length = count / channelCount
                        
                        var inputs: [UnsafePointer<U.Element>] = (0 ..< channelCount).map { i in
                            return inputPointer.advanced(by: Int(i * length))
                        }
                        
                        var outputs: [UnsafeMutablePointer<U.Element>] = (0 ..< channelCount).map { i in
                            return (outputPointer as! UnsafeMutablePointer<U.Element>).advanced(by: Int(i * length))
                        }
                        
                        U.Element.BiquadFunctions.applyMulti(setup: setup,
                                                             pInputs: &inputs,
                                                             pOutputs: &outputs,
                                                             count: count / channelCount)
                    }
                }
        }
        
        static func destroySetup<T: vDSP_FloatingPointBiquadFilterable>(ofType: T.Type,
                                                                        channelCount: UInt,
                                                                        biquadSetup: OpaquePointer) {
            T.BiquadFunctions.destroySetup(channelCount: channelCount,
                                           biquadSetup: biquadSetup)
        }
    }
    
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_FloatingPointBiquadFilterable: BinaryFloatingPoint {
    associatedtype BiquadFunctions: vDSP_BiquadFunctions where BiquadFunctions.Scalar == Self
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Float: vDSP_FloatingPointBiquadFilterable {
    public typealias BiquadFunctions = vDSP.VectorizableFloat
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension Double: vDSP_FloatingPointBiquadFilterable {
    public typealias BiquadFunctions = vDSP.VectorizableDouble
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
public protocol vDSP_BiquadFunctions {
    associatedtype Scalar
    
    /// Returns a data structure that contains precalculated data for use by the cascaded biquad IIR filter function.
    static func makeBiquadSetup(channelCount: vDSP_Length,
                                coefficients: [Double],
                                sectionCount: vDSP_Length) -> OpaquePointer?
    
    /// Applies a single-channel biquad IIR filter.
    static func applySingle<U, V>(source: U,
                                  destination: inout V,
                                  delays: UnsafeMutablePointer<Scalar>,
                                  setup: vDSP_biquad_Setup,
                                  sectionCount: vDSP_Length,
                                  count: vDSP_Length)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Scalar, V.Element == Scalar
    
    /// Applies a multichannel biquad IIR filter.
    static func applyMulti(setup: vDSP_biquadm_SetupD,
                           pInputs: UnsafeMutablePointer<UnsafePointer<Scalar>>,
                           pOutputs: UnsafeMutablePointer<UnsafeMutablePointer<Scalar>>,
                           count: vDSP_Length)
    
    /// Destroys a setup object.
    static func destroySetup(channelCount: UInt,
                             biquadSetup: OpaquePointer)
}

//===----------------------------------------------------------------------===//
//
//  Type-specific Biquad function implementations
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP.VectorizableFloat: vDSP_BiquadFunctions {
    
    /// Returns a data structure that contains precalculated data for use by the cascaded biquad IIR filter function.
    @inlinable
    public static func makeBiquadSetup(channelCount: UInt,
                                       coefficients: [Double],
                                       sectionCount: UInt) -> OpaquePointer? {
        if channelCount == 1 {
            return vDSP_biquad_CreateSetup(coefficients,
                                           sectionCount)
        } else {
            return vDSP_biquadm_CreateSetup(coefficients,
                                            sectionCount,
                                            channelCount)
        }
    }
    
    /// Applies a single-channel biquad IIR filter.
    @inlinable
    public static func applySingle<U, V>(source: U,
                                         destination: inout V,
                                         delays: UnsafeMutablePointer<Scalar>,
                                         setup: vDSP_biquad_Setup,
                                         sectionCount: vDSP_Length,
                                         count: vDSP_Length)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Float, V.Element == Float {
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_biquad(setup,
                                delays,
                                src.baseAddress!, 1,
                                dest.baseAddress!, 1,
                                count)
                }
            }
    }
    
    /// Applies a multichannel biquad IIR filter.
    @inlinable
    public static func applyMulti(setup: vDSP_biquadm_SetupD,
                                  pInputs: UnsafeMutablePointer<UnsafePointer<Scalar>>,
                                  pOutputs: UnsafeMutablePointer<UnsafeMutablePointer<Scalar>>,
                                  count: vDSP_Length) {
        vDSP_biquadm(setup,
                     pInputs, 1,
                     pOutputs, 1,
                     count)
    }
    
    /// Destroys a setup object.
    @inlinable
    public static func destroySetup(channelCount: UInt,
                                    biquadSetup: OpaquePointer) {
        if channelCount == 1 {
            vDSP_biquad_DestroySetup(biquadSetup)
        } else {
            vDSP_biquadm_DestroySetup(biquadSetup)
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vDSP.VectorizableDouble: vDSP_BiquadFunctions {
    
    /// Returns a data structure that contains precalculated data for use by the cascaded biquad IIR filter function.
    @inlinable
    public static func makeBiquadSetup(channelCount: vDSP_Length,
                                       coefficients: [Double],
                                       sectionCount: vDSP_Length) -> OpaquePointer? {
        if channelCount == 1 {
            return vDSP_biquad_CreateSetupD(coefficients,
                                            sectionCount)
        } else {
            return vDSP_biquadm_CreateSetupD(coefficients,
                                             sectionCount,
                                             channelCount)
        }
    }
    
    /// Applies a single-channel biquad IIR filter.
    @inlinable
    public static func applySingle<U, V>(source: U,
                                         destination: inout V,
                                         delays: UnsafeMutablePointer<Scalar>,
                                         setup: vDSP_biquad_Setup,
                                         sectionCount: vDSP_Length,
                                         count: vDSP_Length)
        where
        U: AccelerateBuffer,
        V: AccelerateMutableBuffer,
        U.Element == Double, V.Element == Double {
            
            destination.withUnsafeMutableBufferPointer { dest in
                source.withUnsafeBufferPointer { src in
                    vDSP_biquadD(setup,
                                 delays,
                                 src.baseAddress!, 1,
                                 dest.baseAddress!, 1,
                                 count)
                }
            }
    }
    
    /// Applies a multichannel biquad IIR filter.
    @inlinable
    public static func applyMulti(setup: vDSP_biquadm_SetupD,
                                  pInputs: UnsafeMutablePointer<UnsafePointer<Scalar>>,
                                  pOutputs: UnsafeMutablePointer<UnsafeMutablePointer<Scalar>>,
                                  count: vDSP_Length) {
        vDSP_biquadmD(setup,
                      pInputs, 1,
                      pOutputs, 1,
                      count)
    }
    
    /// Destroys a setup object.
    @inlinable
    public static func destroySetup(channelCount: UInt,
                                    biquadSetup: OpaquePointer) {
        if channelCount == 1 {
            vDSP_biquad_DestroySetupD(biquadSetup)
        } else {
            vDSP_biquadm_DestroySetupD(biquadSetup)
        }
    }
}
