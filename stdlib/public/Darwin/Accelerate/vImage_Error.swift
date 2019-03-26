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

import Accelerate

//===----------------------------------------------------------------------===//
//
//  vImage_Error
//
//===----------------------------------------------------------------------===//

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension vImage {
    /// Returns the description of a vImage error code.
    public static func errorDescription(_ error: vImage_Error) -> String {
        switch error {
        case kvImageNoError:
            return "No Error."
        case kvImageRoiLargerThanInputBuffer:
            return "Roi Larger Than Input Buffer"
        case kvImageInvalidKernelSize:
            return "Invalid Kernel Size"
        case kvImageInvalidEdgeStyle:
            return "Invalid Edge Style"
        case kvImageInvalidOffset_X:
            return "Invalid Offset X"
        case kvImageInvalidOffset_Y:
            return "Invalid Offset Y"
        case kvImageMemoryAllocationError:
            return "Memory Allocation Error"
        case kvImageNullPointerArgument:
            return "Null Pointer Argument"
        case kvImageInvalidParameter:
            return "Invalid Parameter"
        case kvImageBufferSizeMismatch:
            return "Buffer Size Mismatch"
        case kvImageUnknownFlagsBit:
            return "Unknown Flags Bit"
        case kvImageInternalError:
            return "Internal Error"
        case kvImageInvalidRowBytes:
            return "Invalid Row Bytes"
        case kvImageInvalidImageFormat:
            return "Invalid Image Format"
        case kvImageColorSyncIsAbsent:
            return "Color Sync Is Absent"
        case kvImageOutOfPlaceOperationRequired:
            return "Out Of Place Operation Required"
        case kvImageInvalidImageObject:
            return "Invalid Image Object"
        case kvImageInvalidCVImageFormat:
            return "Invalid CVImage Format"
        case kvImageUnsupportedConversion:
            return "Unsupported Conversion"
        case kvImageCoreVideoIsAbsent:
            return "CoreVideo Is Absent"
        case kvImageCVImageFormat_ConversionMatrix:
            return "CVImageFormat Conversion Matrix"
        case kvImageCVImageFormat_ChromaSiting:
            return "CVImageFormat Chroma Siting"
        case kvImageCVImageFormat_ColorSpace:
            return "CVImageFormat Color Space"
        case kvImageCVImageFormat_VideoChannelDescription:
            return "CVImageFormat Video Channel Description"
        case kvImageCVImageFormat_AlphaIsOneHint:
            return "CVImageFormat Alpha Is One Hint"
        default:
            return "Unknown error."
        }
    }
}
