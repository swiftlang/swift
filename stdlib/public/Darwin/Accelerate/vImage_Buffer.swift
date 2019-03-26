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
//  vImage_Buffer
//
//===----------------------------------------------------------------------===//

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension vImage_Buffer {
    
    /// Returns the size, in pixels, of a vImage buffer.
    public var size: CGSize {
        var mutableSelf = self
        return vImageBuffer_GetSize(&mutableSelf)
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Initializes a vImage buffer of a specified size without memory allocation,
    /// reporting the preferred alignment.
    ///
    /// - Parameter width: The width of the image.
    /// - Parameter height: The height of the image.
    /// - Parameter bitsPerPixel: The number of bits in a pixel of image data.
    /// - Parameter alignment: The preferred alignment is written to this parameter.
    ///
    /// - Returns: An initialized vImage buffer.
    public init?(size: CGSize,
                 bitsPerPixel: UInt32,
                 alignment: inout Int) {
        
        self.init()
        
        if size.width < 0 || size.height < 0 {
            return nil
        }
        let width = vImagePixelCount(size.width)
        let height = vImagePixelCount(size.height)
        
        let error = vImageBuffer_Init(&self,
                                      height,
                                      width,
                                      bitsPerPixel,
                                      vImage_Flags(kvImageNoAllocate))
        
        if error < kvImageNoError {
            return nil
        } else {
            alignment = error
        }
    }
    
    /// Initializes a vImage buffer of a specified size, reporting any errors.
    ///
    /// - Parameter width: The width of the image.
    /// - Parameter height: The height of the image.
    /// - Parameter bitsPerPixel: The number of bits in a pixel of image data.
    /// - Parameter error: Overwritten with the error code if the operation failed.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    public init?(size: CGSize,
                 bitsPerPixel: UInt32,
                 error: inout Int,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        if size.width < 0 || size.height < 0 {
            error = kvImageInvalidParameter
            return nil
        }
        let width = vImagePixelCount(size.width)
        let height = vImagePixelCount(size.height)
        
        error = vImageBuffer_Init(&self,
                                  height,
                                  width,
                                  bitsPerPixel,
                                  options.flags)
        
        if error < kvImageNoError {
            return nil
        }
    }
    
    /// Initializes a vImage buffer of a specified size.
    ///
    /// - Parameter width: The width of the image.
    /// - Parameter height: The height of the image.
    /// - Parameter bitsPerPixel: The number of bits in a pixel of image data.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    public init?(size: CGSize,
                 bitsPerPixel: UInt32,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        if size.width < 0 || size.height < 0 {
            return nil
        }
        let width = vImagePixelCount(size.width)
        let height = vImagePixelCount(size.height)
        
        let error = vImageBuffer_Init(&self,
                                      height,
                                      width,
                                      bitsPerPixel,
                                      options.flags)
        
        if error < kvImageNoError {
            return nil
        }
    }
    
    public func free() {
        Darwin.free(data)
    }
}

// MARK: Core Graphics Support

@available(iOS 9999, OSX 9999, tvOS 9999, watchOS 9999, *)
extension vImage_Buffer {
    
    /// Initialize a vImage buffer with the contents of a Core Graphics image, reporting any errors.
    ///
    /// - Parameter cgImage: A `CGImage` instance to be used as the source.
    /// - Parameter error: Overwritten with the error code if the operation failed.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    ///
    /// This function will instantiate and initialize a vImage buffer from a `CGImage` using a `CGImageFormat`
    /// based on the provided image's properties.
    public init?(cgImage: CGImage,
                 error: inout Int,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        guard var format = vImage_CGImageFormat(cgImage: cgImage) else {
            return nil
        }
        
        error = vImageBuffer_InitWithCGImage(&self,
                                             &format,
                                             nil,
                                             cgImage,
                                             options.flags)
        
        if error != kvImageNoError {
            return nil
        }
    }
    
    /// Initialize a vImage buffer with the contents of a Core Graphics image.
    ///
    /// - Parameter cgImage: A `CGImage` instance to be used as the source.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    ///
    /// This function will instantiate and initialize a vImage buffer from a `CGImage` using a `CGImageFormat` based on the provided image's properties.
    public init?(cgImage: CGImage,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        guard var format = vImage_CGImageFormat(cgImage: cgImage) else {
            return nil
        }
        
        let error = vImageBuffer_InitWithCGImage(&self,
                                                 &format,
                                                 nil,
                                                 cgImage,
                                                 options.flags)
        
        if error != kvImageNoError {
            return nil
        }
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Initialize a vImage buffer with the contents of a Core Graphics image,
    /// using a supplied format and reporting any errors.
    ///
    /// - Parameter cgImage: A `CGImage` instance to be used as the source.
    /// - Parameter format: A `vImage_CGImageFormat` that describes the source image.
    /// - Parameter error: Overwritten with the error code if the operation failed.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: An initialized vImage buffer.
    ///
    /// This function will instantiate and initialize a vImage buffer from a `CGImage` using a provided `CGImageFormat`.
    public init?(cgImage: CGImage,
                 format: vImage_CGImageFormat,
                 error: inout Int,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        var format = format
        error = vImageBuffer_InitWithCGImage(&self,
                                             &format,
                                             nil,
                                             cgImage,
                                             options.flags)
        
        if error != kvImageNoError {
            return nil
        }
    }
    
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
    public init?(cgImage: CGImage,
                 format: vImage_CGImageFormat,
                 flags options: vImage.Options = .noFlags) {
        
        self.init()
        
        var format = format
        let error = vImageBuffer_InitWithCGImage(&self,
                                                 &format,
                                                 nil,
                                                 cgImage,
                                                 options.flags)
        
        if error != kvImageNoError {
            return nil
        }
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Creates a `CGImage` instance from a vImage buffer, reporting any errors.
    ///
    /// - Parameter format: The image format of this vImage buffer.
    /// - Parameter error: Overwritten with the error code if the operation failed.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: A Core Graphics image containing a representation of the vImage buffer.
    public func createCGImage(format: vImage_CGImageFormat,
                              error: inout Int,
                              flags options: vImage.Options = .noFlags) -> CGImage? {
        var format = format
        
        var cgImage: CGImage!
        
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
            return nil
        }
        
        return cgImage
    }
    
    /// Creates a `CGImage` instance from a vImage buffer
    ///
    /// - Parameter format: The image format of this vImage buffer.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: A Core Graphics image containing a representation of the vImage buffer.
    public func createCGImage(format: vImage_CGImageFormat,
                              flags options: vImage.Options = .noFlags) -> CGImage? {
        var format = format
        var error = kvImageNoError
        
        var cgImage: CGImage!
        
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
            return nil
        }
        
        return cgImage
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Copies this buffer to `destinationBuffer`.
    ///
    /// - Parameter destinationBuffer: The destination vImage buffer.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: `kvImageNoError`; otherwise, one of the error codes described
    ///             in Data Types and Constants.
    public func copy(destinationBuffer: inout vImage_Buffer,
                     flags options: vImage.Options = .noFlags) -> vImage_Error? {
        
        if Int(width) == 0 {
            return kvImageInvalidParameter
        }
        
        var error = kvImageNoError
        
        _ = withUnsafePointer(to: self) {
            error =  vImageCopyBuffer($0,
                                      &destinationBuffer,
                                      rowBytes / Int(width),
                                      options.flags)
        }
        
        return error
    }
    
}
