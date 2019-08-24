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
//  vImage_CGImageFormat
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage_CGImageFormat {
    
    /// Initializes an image format from a Core Graphics image.
    ///
    /// - Parameter cgImage: The image from which to derive the image format.
    ///
    /// - Returns: An initialized `vImage_CGImageFormat`.
    public init?(cgImage: CGImage) {
        guard
            let colorSpace = cgImage.colorSpace else {
                return nil
        }
        
        self = vImage_CGImageFormat(
            bitsPerComponent: UInt32(cgImage.bitsPerComponent),
            bitsPerPixel: UInt32(cgImage.bitsPerPixel),
            colorSpace: Unmanaged.passRetained(colorSpace),
            bitmapInfo: cgImage.bitmapInfo,
            version: 0,
            decode: nil,
            renderingIntent: cgImage.renderingIntent)
    }
    
    /// Initializes an image format.
    ///
    /// - Parameter bitsPerComponent: The number of bits needed to represent one
    /// channel of data in one pixel.
    /// - Parameter bitsPerPixel: The number of bits needed to represent one pixel.
    /// - Parameter colorSpace: The color space for the format.
    /// - Parameter bitmapInfo: The component information describing the color channels.
    /// - Parameter renderingIntent: A rendering intent constant that specifies how
    /// Core Graphics should handle colors that are not located within the gamut of the
    /// destination color space of a graphics context.
    ///
    /// - Returns: An initialized `vImage_CGImageFormat`.
    public init?(bitsPerComponent: Int,
                 bitsPerPixel: Int,
                 colorSpace: CGColorSpace,
                 bitmapInfo: CGBitmapInfo,
                 renderingIntent: CGColorRenderingIntent = .defaultIntent) {
        
        if bitsPerComponent < 1 || bitsPerPixel < 0 {
            return nil
        }
        
        self = vImage_CGImageFormat(
            bitsPerComponent: UInt32(bitsPerComponent),
            bitsPerPixel: UInt32(bitsPerPixel),
            colorSpace: Unmanaged.passRetained(colorSpace),
            bitmapInfo: bitmapInfo,
            version: 0,
            decode: nil,
            renderingIntent: renderingIntent)
    }
    
    /// The number of color channels.
    public var componentCount: Int {
        var mutableSelf = self
        return Int(vImageCGImageFormat_GetComponentCount(&mutableSelf))
    }
}

