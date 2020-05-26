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
//  vImage_Buffer
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage_Buffer {
    
    /// The size of the vImage buffer.
    ///
    /// The `CGSize` is rounded down to the nearest representable `CGFloat` that
    /// is less than or equal to the actual size of the image. In practice the
    /// conversion will always be exact, except for really big images. In that
    /// case, some part of the bottom or right edge might be truncated.
    public var size: CGSize {
        var mutableSelf = self
        return vImageBuffer_GetSize(&mutableSelf)
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Returns the preferred alignment and row bytes for a specified buffer
    /// size and bits-per-pixel.
    ///
    /// - Parameter width: The width of the buffer.
    /// - Parameter height: The height of the buffer.
    /// - Parameter bitsPerPixel: The number of bits in a pixel of image data.
    ///
    /// - Returns: The preferred alignment and row bytes.
    public static func preferredAlignmentAndRowBytes(width: Int,
                                                     height: Int,
                                                     bitsPerPixel: UInt32) throws -> (alignment: Int, rowBytes: Int) {
        
        if width < 0 || height < 0 {
            throw vImage.Error.invalidParameter
        }
        
        var buffer = vImage_Buffer()
        
        let error = vImageBuffer_Init(&buffer,
                                      vImagePixelCount(height),
                                      vImagePixelCount(width),
                                      bitsPerPixel,
                                      vImage_Flags(kvImageNoAllocate))
        
        if error < kvImageNoError {
            throw vImage.Error(vImageError: error)
        } else {
            return(alignment: error,
                   rowBytes: buffer.rowBytes)
        }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  Initializers.
    //
    //===----------------------------------------------------------------------===//
    
    /// Initializes a vImage buffer of a specified size.
    ///
    /// - Parameter width: The width of the buffer.
    /// - Parameter height: The height of the buffer.
    /// - Parameter bitsPerPixel: The number of bits in a pixel of image data.
    ///
    /// - Returns: An initialized vImage buffer.
    public init(width: Int,
                height: Int,
                bitsPerPixel: UInt32) throws {
        
        if width < 0 || height < 0 {
            throw vImage.Error.invalidParameter
        }
        
        self.init()
        
        let error = vImageBuffer_Init(&self,
                                      vImagePixelCount(height),
                                      vImagePixelCount(width),
                                      bitsPerPixel,
                                      vImage_Flags(kvImageNoFlags))
        
        if error < kvImageNoError {
            throw vImage.Error(vImageError: error)
        }
    }
    
    public func free() {
        Darwin.free(data)
    }
}

// MARK: Core Graphics Support

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage_Buffer {
    
    /// Initialize a vImage buffer with the contents of a Core Graphics image.
    ///
    /// - Parameter cgImage: A `CGImage` instance to be used as the source.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    ///
    /// This function will instantiate and initialize a vImage buffer from a `CGImage` using a `CGImageFormat` based on the provided image's properties.
    public init(cgImage: CGImage,
                flags options: vImage.Options = .noFlags) throws {
        
        self.init()
        
        guard var format = vImage_CGImageFormat(cgImage: cgImage) else {
            throw vImage.Error.invalidImageFormat
        }
        
        let error = vImageBuffer_InitWithCGImage(&self,
                                                 &format,
                                                 nil,
                                                 cgImage,
                                                 options.flags)
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        }
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Initialize a vImage buffer with the contents of a Core Graphics image,
    /// using a supplied format.
    ///
    /// - Parameter cgImage: A `CGImage` instance to be used as the source.
    /// - Parameter format: A `vImage_CGImageFormat` that describes the source image/
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    ///
    /// This function will instantiate and initialize a vImage buffer from a `CGImage` using a provided `CGImageFormat`.
    public init(cgImage: CGImage,
                format: vImage_CGImageFormat,
                flags options: vImage.Options = .noFlags) throws {
        
        self.init()
        
        var format = format
        let error = vImageBuffer_InitWithCGImage(&self,
                                                 &format,
                                                 nil,
                                                 cgImage,
                                                 options.flags)
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        }
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Creates a `CGImage` instance from a vImage buffer
    ///
    /// - Parameter format: The image format of this vImage buffer.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: A Core Graphics image containing a representation of the vImage buffer.
    public func createCGImage(format: vImage_CGImageFormat,
                              flags options: vImage.Options = .noFlags) throws -> CGImage {
        var format = format
        var error = kvImageNoError
        
        var cgImage: CGImage?
        
        withUnsafePointer(to: self) {
            cgImage = vImageCreateCGImageFromBuffer(
                $0,
                &format,
                nil,
                nil,
                options.flags,
                &error).takeRetainedValue()
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        } else if cgImage == nil {
            throw vImage.Error.internalError
        }
        
        return cgImage!
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Copies this buffer to `destinationBuffer`.
    ///
    /// - Parameter destinationBuffer: The destination vImage buffer.
    /// - Parameter pixelSize: The number of bytes for one pixel.
    /// - Parameter options: The options to use when performing this operation.
    public func copy(destinationBuffer: inout vImage_Buffer,
                     pixelSize: Int,
                     flags options: vImage.Options = .noFlags) throws {
        
        if Int(width) == 0 {
            throw vImage.Error(vImageError: kvImageInvalidParameter)
        }

        var error = kvImageNoError
        
        withUnsafePointer(to: self) {
            error =  vImageCopyBuffer($0,
                                      &destinationBuffer,
                                      pixelSize,
                                      options.flags)
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        }
    }
    
}
