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
//  vImage_Error
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage {
    
    /// Error codes returned by vImage operations.
    public enum Error: Int, Swift.Error {
        /// The vImage function completed without error.
        case noError                      =    0
        
        // The region of interest, as specified by the `srcOffsetToROI_X` and
        // `srcOffsetToROI_Y` parameters and the height and width of the destination
        // buffer, extends beyond the bottom edge or right edge of the source buffer.
        case roiLargerThanInputBuffer     =    -21766
        
        /// Either the kernel height, the kernel width, or both, are even.
        case invalidKernelSize            =    -21767
        
        /// The edge style specified is invalid.
        case invalidEdgeStyle             =    -21768
        
        /// The `srcOffsetToROI_X` parameter that specifies the left edge of
        /// the region of interest is greater than the width of the source image.
        case invalidOffset_X              =    -21769
        
        /// The `srcOffsetToROI_X` parameter that specifies the left edge of
        /// the region of interest is greater than the height of the source image.
        case invalidOffset_Y              =    -21770
        
        /// An attempt to allocate memory failed.
        case memoryAllocationError        =    -21771
        
        /// A pointer parameter is NULL and it must not be.
        case nullPointerArgument          =    -21772
        
        /// Invalid parameter.
        case invalidParameter             =    -21773
        
        /// The function requires the source and destination buffers to have
        /// the same height and the same width, but they do not.
        case bufferSizeMismatch           =    -21774
        
        /// The flag is not recognized.
        case unknownFlagsBit              =    -21775
        
        /// A serious error occured inside vImage, which prevented vImage
        /// from continuing.
        case internalError                =    -21776
        
        /// The vImage_Buffer.rowBytes field is invalid.
        case invalidRowBytes              =    -21777
        
        /// a `vImage_CGImageFormat` or `vImageCVImageFormatRef` contains
        /// an invalid format.
        case invalidImageFormat           =    -21778
        
        /// ColorSync.framework is completely missing.
        case colorSyncIsAbsent            =    -21779
        
        /// The source images and destination images may not alias the same image data.
        case outOfPlaceOperationRequired  =    -21780
        
        /// An invalid `CGImageRef` or `CVPixelBufferRef` was passed to the function.
        case invalidImageObject           =    -21781
        
        /// A `vImageCVImageFormatRef` contains an invalid format.
        case invalidCVImageFormat         =    -21782
        
        /// Some lower level conversion APIs only support conversion among a
        /// sparse matrix of image formats.
        case unsupportedConversion        =    -21783
        
        /// Core Video is absent.
        case coreVideoIsAbsent            =    -21784
        
        public init(vImageError: vImage_Error) {
            self = Error(rawValue: vImageError) ?? .internalError
        }
    }
    
}
