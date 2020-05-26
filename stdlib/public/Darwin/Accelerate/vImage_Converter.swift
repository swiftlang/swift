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
//  vImageConverter
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImageConverter {
    
    //===----------------------------------------------------------------------===//
    
    /// Returns an array of vImage source buffer types specifying the order of planes.
    ///
    /// - Parameter colorSpace: The color space of the source format.
    public func sourceBuffers(colorSpace: CGColorSpace) -> [vImage.BufferType?]  {
        let sourceBuffers = vImageConverter_GetSourceBufferOrder(self)
        let codes = Array(UnsafeBufferPointer(start: sourceBuffers,
                                              count: sourceBufferCount))
        
        return codes.map {
            vImage.BufferType(bufferTypeCode: Int($0),
                              model: colorSpace.model)
        }
    }
    
    /// Returns an array of vImage source buffer types specifying the order of planes.
    ///
    /// - Parameter colorSpace: The color space of the destination format.
    public func destinationBuffers(colorSpace: CGColorSpace) -> [vImage.BufferType?]  {
        let destinationBuffers = vImageConverter_GetDestinationBufferOrder(self)
        let codes = Array(UnsafeBufferPointer(start: destinationBuffers,
                                              count: destinationBufferCount))
        
        return codes.map {
            vImage.BufferType(bufferTypeCode: Int($0),
                              model: colorSpace.model)
        }
    }
    
    /// The number of source buffers consumed by the converter.
    public var sourceBufferCount: Int {
        return Int(vImageConverter_GetNumberOfSourceBuffers(self))
    }
    
    /// The number of destination buffers written to by the converter.
    public var destinationBufferCount: Int {
        return Int(vImageConverter_GetNumberOfDestinationBuffers(self))
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Determines whether a converter is capable of operating in place.
    ///
    /// - Parameter source: The source buffer.
    /// - Parameter destination: The destination buffer.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: `true` if the conversion must operate out of place or `false`
    /// if the operation will work in place.
    public func mustOperateOutOfPlace(source: vImage_Buffer,
                                      destination: vImage_Buffer,
                                      flags options: vImage.Options = .noFlags) throws -> Bool {
        var error = kvImageNoError
        
        withUnsafePointer(to: source) { src in
            withUnsafePointer(to: destination) { dest in
                error = vImageConverter_MustOperateOutOfPlace(self,
                                                              src,
                                                              dest,
                                                              vImage_Flags(options.rawValue))
            }
        }
        
        switch error {
        case kvImageOutOfPlaceOperationRequired:
            return true
        case kvImageNoError:
            return false
        default:
            throw vImage.Error(vImageError: error)
        }
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: CG -> CG
    //
    //===----------------------------------------------------------------------===//
    
    /// Creates a vImage converter to convert from one vImage Core Graphics image format to another.
    ///
    /// - Parameter sourceFormat: A `vImage_CGImageFormat` structure describing the image format of the source image
    /// - Parameter destinationFormat: A `vImage_CGImageFormat` structure describing the image format of the destination image
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: a vImage converter to convert from one vImage Core Graphics image format to another.
    public static func make(sourceFormat: vImage_CGImageFormat,
                            destinationFormat: vImage_CGImageFormat,
                            flags options: vImage.Options = .noFlags) throws -> vImageConverter {
        
        var error = kvImageNoError
        var unmanagedConverter: Unmanaged<vImageConverter>?
        
        withUnsafePointer(to: destinationFormat) { dest in
            withUnsafePointer(to: sourceFormat) { src in
                unmanagedConverter = vImageConverter_CreateWithCGImageFormat(
                    src,
                    dest,
                    nil,
                    vImage_Flags(options.rawValue),
                    &error)
            }
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        } else if unmanagedConverter == nil {
            throw vImage.Error.internalError
        }
        
        return unmanagedConverter!.takeRetainedValue()
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: CG -> CV
    //
    //===----------------------------------------------------------------------===//
    
    /// Creates a vImage converter that converts a Core Graphics-formatted image to a Core Video-formatted image.
    ///
    /// - Parameter sourceFormat: A `vImage_CGImageFormat` structure describing the image format of the source image
    /// - Parameter destinationFormat: A `vImageCVImageFormat` structure describing the image format of the destination image
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: a vImage converter to convert a Core Graphics-formatted image to a Core Video-formatted image.
    public static func make(sourceFormat: vImage_CGImageFormat,
                            destinationFormat: vImageCVImageFormat,
                            flags options: vImage.Options = .noFlags) throws -> vImageConverter {
        
        var error = kvImageNoError
        var unmanagedConverter: Unmanaged<vImageConverter>?
        
        withUnsafePointer(to: sourceFormat) { src in
            unmanagedConverter = vImageConverter_CreateForCGToCVImageFormat(
                src,
                destinationFormat,
                nil,
                vImage_Flags(options.rawValue),
                &error)
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        } else if unmanagedConverter == nil {
            throw vImage.Error.internalError
        }
        
        return unmanagedConverter!.takeRetainedValue()
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: CV -> CG
    //
    //===----------------------------------------------------------------------===//
    
    /// Creates a vImage converter that converts a Core Video-formatted image to a Core Graphics-formatted image.
    ///
    /// - Parameter sourceFormat: A `vImageCVImageFormat` structure describing the image format of the source image
    /// - Parameter destinationFormat: A `vImage_CGImageFormat` structure describing the image format of the destination image
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: a vImage converter to convert a Core Video-formatted image to a Core Graphics-formatted image.
    public static func make(sourceFormat: vImageCVImageFormat,
                            destinationFormat: vImage_CGImageFormat,
                            flags options: vImage.Options = .noFlags) throws -> vImageConverter {
        
        var error = kvImageInternalError
        var unmanagedConverter: Unmanaged<vImageConverter>?
        
        withUnsafePointer(to: destinationFormat) { dest in
            unmanagedConverter = vImageConverter_CreateForCVToCGImageFormat(
                sourceFormat,
                dest,
                nil,
                vImage_Flags(options.rawValue),
                &error)
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        } else if unmanagedConverter == nil {
            throw vImage.Error.internalError
        }
        
        return unmanagedConverter!.takeRetainedValue()
    }
    
    //===----------------------------------------------------------------------===//
    
    /// Converts the pixels in a vImage buffer to another format.
    ///
    /// - Parameter source: The source buffer.
    /// - Parameter destination: The destination buffer.
    /// - Parameter options: The options to use when performing this operation.
    ///
    /// - Returns: `kvImageNoError`; otherwise, one of the error codes described in _Data Types and Constants.
    public func convert(source: vImage_Buffer,
                        destination: inout vImage_Buffer,
                        flags options: vImage.Options = .noFlags) throws {
        
        var error = kvImageNoError
        
        withUnsafePointer(to: source) { src in
            error = vImageConvert_AnyToAny(self,
                                           src,
                                           &destination,
                                           nil,
                                           vImage_Flags(options.rawValue))
        }
        
        if error != kvImageNoError {
            throw vImage.Error(vImageError: error)
        }
    }
}

