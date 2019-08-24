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
//  vImage_Options
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage {
    
    /// An option set that represents `vImage_Flags`.
    ///
    /// You can also use this option set with the existing vImage API with the
    /// `flags` property. For example:
    ///
    ///     vImageTentConvolve_ARGB8888(&src, &dst, nil,
    ///                                0, 0, 11, 11,
    ///                                 [0,0,0,0],
    ///                                 vImage_Options([.backgroundColorFill,
    ///                                                 .doNotTile]).flags)
    public struct Options: OptionSet {
        
        public init(rawValue: vImage_Flags) {
            self.rawValue = rawValue
        }
        
        public let rawValue: vImage_Flags
        
        public static let noFlags = Options(rawValue: vImage_Flags(kvImageNoFlags))
        
        /// Operate on red, green and blue channels only. Alpha is copied from source
        /// to destination. For Interleaved formats only
        public static let leaveAlphaUnchanged = Options(rawValue: vImage_Flags(kvImageLeaveAlphaUnchanged))
        
        /// Copy edge pixels. Convolution Only.
        public static let copyInPlace = Options(rawValue: vImage_Flags(kvImageCopyInPlace))
        
        /// Use the background color for missing pixels.
        public static let backgroundColorFill = Options(rawValue: vImage_Flags(kvImageBackgroundColorFill))
        
        /// Use the nearest pixel for missing pixels.
        public static let imageExtend = Options(rawValue: vImage_Flags(kvImageEdgeExtend))
        
        /// Pass to turn off internal tiling and disable internal multithreading. Use this if
        /// you want to do your own tiling, or to use the Min/Max filters in place.
        public static let doNotTile = Options(rawValue: vImage_Flags(kvImageDoNotTile))
        
        /// Use a higher quality, slower resampling filter for Geometry operations
        /// (shear, scale, rotate, affine transform, etc.)
        public static let highQualityResampling = Options(rawValue: vImage_Flags(kvImageHighQualityResampling))
        
        /// Use only the part of the kernel that overlaps the image. For integer kernels,
        /// real_divisor = divisor * (sum of used kernel elements) / (sum of kernel elements).
        /// This should preserve image brightness at the edges. Convolution only.
        public static let truncateKernel = Options(rawValue: vImage_Flags(kvImageTruncateKernel))
        
        /// The function will return the number of bytes required for the temp buffer.
        /// If this value is negative, it is an error, per standard usage.
        public static let getTempBufferSize = Options(rawValue: vImage_Flags(kvImageGetTempBufferSize))
        
        /// Some functions such as vImageConverter_CreateWithCGImageFormat have so many possible error conditions
        /// that developers may need more help than a simple error code to diagnose problems. When this
        /// flag is set and an error is encountered, an informative error message will be logged to the Apple
        /// System Logger (ASL).  The output should be visible in Console.app.
        public static let printDiagnosticsToConsole = Options(rawValue: vImage_Flags(kvImagePrintDiagnosticsToConsole))
        
        /// Pass this flag to prevent vImage from allocating additional storage.
        public static let noAllocate = Options(rawValue: vImage_Flags(kvImageNoAllocate))
        
        /// Use methods that are HDR-aware, capable of providing correct results for input images with pixel values
        /// outside the otherwise limited (typically [-2,2]) range. This may be slower.
        public static let hdrContent = Options(rawValue: vImage_Flags(kvImageHDRContent))
        
        /// Pass to disable clamping is some conversions to floating point formats. Use this if the input data
        /// may describe values outside [0,1] which should be preserved.
        public static let doNotClamp = Options(rawValue: vImage_Flags(kvImageDoNotClamp))
        
        public var flags: vImage_Flags {
            return self.rawValue
        }
    }
    
}
