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
    
    /// Converts split complex to interleaved complex; single-precision.
    ///
    /// - Parameter splitComplexVector: Source vector.
    /// - Parameter interleavedComplexVector: Destination vector.
    @inlinable
    public static func convert(splitComplexVector: DSPSplitComplex,
                               toInterleavedComplexVector interleavedComplexVector: inout [DSPComplex]) {
        
        withUnsafePointer(to: splitComplexVector) {
            vDSP_ztoc($0, 1,
                      &interleavedComplexVector, 2,
                      vDSP_Length(interleavedComplexVector.count))
        }
    }
    
    /// Converts interleaved complex to split complex; single-precision.
    ///
    /// - Parameter interleavedComplexVector: Source vector.
    /// - Parameter splitComplexVector: Destination vector.
    @inlinable
    public static func convert(interleavedComplexVector: [DSPComplex],
                               toSplitComplexVector splitComplexVector: inout DSPSplitComplex) {
        
        vDSP_ctoz(interleavedComplexVector, 2,
                  &splitComplexVector, 1,
                  vDSP_Length(interleavedComplexVector.count))
    }
    
    /// Converts split complex to interleaved complex; double-precision.
    ///
    /// - Parameter splitComplexVector: Source vector.
    /// - Parameter interleavedComplexVector: Destination vector.
    @inlinable
    public static func convert(splitComplexVector: DSPDoubleSplitComplex,
                               toInterleavedComplexVector interleavedComplexVector: inout [DSPDoubleComplex]) {
        
        withUnsafePointer(to: splitComplexVector) {
            vDSP_ztocD($0, 1,
                       &interleavedComplexVector, 2,
                       vDSP_Length(interleavedComplexVector.count))
        }
    }
    
    /// Converts interleaved complex to split complex; double-precision.
    ///
    /// - Parameter interleavedComplexVector: Source vector.
    /// - Parameter splitComplexVector: Destination vector.
    @inlinable
    public static func convert(interleavedComplexVector: [DSPDoubleComplex],
                               toSplitComplexVector splitComplexVector: inout DSPDoubleSplitComplex) {
        
        vDSP_ctozD(interleavedComplexVector, 2,
                   &splitComplexVector, 1,
                   vDSP_Length(interleavedComplexVector.count))
    }
}
