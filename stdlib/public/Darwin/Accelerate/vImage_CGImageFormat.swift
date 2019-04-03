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
//  vImage_CGImageFormat
//
//===----------------------------------------------------------------------===//

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension vImage_CGImageFormat {
    
    /// Initializes an image format from a Core Graphics image.
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
    public init(bitsPerComponent: UInt32,
                bitsPerPixel: UInt32,
                colorSpace: CGColorSpace,
                bitmapInfo: CGBitmapInfo,
                renderingIntent: CGColorRenderingIntent = .defaultIntent) {
        
        self = vImage_CGImageFormat(
            bitsPerComponent: bitsPerComponent,
            bitsPerPixel: bitsPerPixel,
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
    
    /// Test to see if two vImage_CGImageFormats are equivalent.
    public static func == (lhs: vImage_CGImageFormat, rhs: vImage_CGImageFormat) -> Bool {
        var mutableLhs = lhs
        var mutableRhs = rhs
        
        return vImageCGImageFormat_IsEqual(&mutableLhs, &mutableRhs)
    }
}

