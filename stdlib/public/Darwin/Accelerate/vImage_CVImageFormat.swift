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
//  vImageCVImageFormat
//
//===----------------------------------------------------------------------===//

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImageCVImageFormat {
    
    /// Creates the description of how an image is encoded in a Core Video pixel buffer.
    ///
    /// - Parameter format: The format type of the image.
    /// - Parameter matrix: A `vImage_ARGBToYpCbCrMatrix` that describes the conversion from RGB to the YpCbCr format.
    /// - Parameter chromaSiting: The chroma location.
    /// - Parameter colorSpace: The color space of the RGB and monochrome images.
    /// - Parameter alphaIsOpaqueHint: A hint indicating that an image with an alpha channel should be treated as opaque.
    ///
    /// - Returns: A `vImageCVImageFormatRef` instance encoded with the function's parameters.
    public static func make(format: Format,
                            matrix: vImage_ARGBToYpCbCrMatrix,
                            chromaSiting: ChromaSiting,
                            colorSpace: CGColorSpace,
                            alphaIsOpaqueHint: Bool) -> vImageCVImageFormat? {
        
        var mutableMatrix = matrix
        
        return vImageCVImageFormat_Create(format.ostype,
                                          &mutableMatrix,
                                          chromaSiting.cfString,
                                          colorSpace,
                                          alphaIsOpaqueHint ? 1 : 0)?.takeRetainedValue()
    }
    
    /// Creates the description of how an image is encoded in an existing Core Video pixel buffer.
    ///
    /// - Parameter buffer: The CVPixelBufferRef on which to base the returned vImageCVImageFormatRef.
    ///
    /// - Returns: A `vImageCVImageFormatRef` instance encoded with the supplied pixel buffer's pixel format.
    public static func make(buffer: CVPixelBuffer) -> vImageCVImageFormat? {
        
        return vImageCVImageFormat_CreateWithCVPixelBuffer(buffer).takeRetainedValue()
    }
    
    ///  The alpha hint of a Core Video image format.
    public var alphaIsOpaqueHint: Bool {
        get {
            return vImageCVImageFormat_GetAlphaHint(cvConstImageFormat) != 0
        }
        set {
            let error = vImageCVImageFormat_SetAlphaHint(self,
                                                         newValue ? 2 : 0)
            
            if error != kvImageNoError {
                fatalError("Unable to set `alphaIsOpaqueHint`")
            }
        }
    }
    
    /// The number of channels, including alpha, for the Core Video image format.
    public var channelCount: UInt32  {
        return vImageCVImageFormat_GetChannelCount(cvConstImageFormat)
    }
    
    /// The names of the channels of a Core Video image format.
    public var channels: [vImage.BufferType] {
        let channels = Array(UnsafeBufferPointer(start: vImageCVImageFormat_GetChannelNames(cvConstImageFormat),
                                                 count: Int(channelCount)))
        
        return channels.compactMap {
            return vImage.BufferType(bufferTypeCode: Int($0),
                                     model: colorSpace?.model)
        }
    }
    
    /// Returns the channel description for a particular channel type.
    public func channelDescription(bufferType: vImage.BufferType) -> vImageChannelDescription? {
        guard let description = vImageCVImageFormat_GetChannelDescription(cvConstImageFormat,
                                                                          bufferType.bufferTypeCode) else {
                                                                            return nil
        }
        
        return vImageChannelDescription(min: description.pointee.min,
                                        zero: description.pointee.zero,
                                        full: description.pointee.full,
                                        max:  description.pointee.max)
    }
    
    /// The chroma siting of a Core Video image format.
    public var chromaSiting: ChromaSiting?  {
        get {
            return ChromaSiting(location: vImageCVImageFormat_GetChromaSiting(cvConstImageFormat)?.takeRetainedValue())
        }
        set {
            let error = vImageCVImageFormat_SetChromaSiting(self,
                                                            newValue?.cfString)
            
            if error != kvImageNoError {
                fatalError("Unable to set `chromaSiting`")
            }
        }
    }
    
    ///  The color space of a Core Video image format.
    public var colorSpace: CGColorSpace?  {
        get {
            return vImageCVImageFormat_GetColorSpace(cvConstImageFormat)?.takeRetainedValue()
        }
        set {
            let error = vImageCVImageFormat_SetColorSpace(self,
                                                          newValue)
            
            if error != kvImageNoError {
                fatalError("Unable to set `colorSpace`")
            }
        }
    }
    
    /// The format code of a Core Video image format.
    public var formatCode: UInt32 {
        return vImageCVImageFormat_GetFormatCode(cvConstImageFormat)
    }
    
    /// Utility to return immuatable copy of self for use in getters.
    private var cvConstImageFormat: vImageConstCVImageFormat {
        
        let cvImageFormatPointer = UnsafeMutableRawPointer.allocate(byteCount: MemoryLayout<vImageCVImageFormat>.size,
                                                                    alignment: MemoryLayout<vImageCVImageFormat>.alignment)
        cvImageFormatPointer.storeBytes(of: self,
                                        as: vImageCVImageFormat.self)
        return cvImageFormatPointer.load(as: vImageConstCVImageFormat.self)
    }
    
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImage {
    /// Type codes for what is in a `vImage_Buffer`, such as red or luminance.
    public enum BufferType: Int {
        
        /// The buffer contains the alpha channel / coverage component
        case alpha
        
        /// The buffer contains data describable as a `vImage_CGImageFormat` as a
        /// single (likely chunky) buffer.
        case coreGraphics
        
        /// If the image has a CMYK color model, the buffer contains the black channel.
        case cmykBlack
        
        /// If the image has a CMYK color model, the buffer contains the cyan channel.
        case cmykCyan
        
        /// If the image has a CMYK color model, the buffer contains the magenta channel.
        case cmykMagenta
        
        /// If the image has a CMYK color model, the buffer contains the yellow channel.
        case cmykYellow
        
        /// The buffer contains luminance, and both chroma channels interleaved
        /// according to the vImageConstCVImageFormatRef image type.
        case YCbCr
        
        /// The buffer contains the blue chrominance channel.
        case Cb
        
        /// The buffer contains the red chrominance channel.
        case Cr
        
        /// The buffer contains both chrominance channels, interleaved.
        case chroma
        
        /// The buffer contains chunky data not describable as a `vImage_CGImageFormat`.
        case chunky
        
        /// The buffer contains data in an indexed colorspace.
        case indexed
        
        /// If the image has a LAB color model, the buffer contains the a* channel.
        case labA
        
        /// If the image has a LAB color model, the buffer contains the b* channel.
        case labB
        
        /// If the image has a LAB color model, the buffer contains the L* channel.
        case labL
        
        /// The buffer contains only luminance data.
        case luminance
        
        /// The buffer contains monochrome data.
        case monochrome
        
        /// If the image has a RGB color model, the buffer contains the red channel.
        case rgbRed
        
        /// If the image has a RGB color model, the buffer contains the green channel.
        case rgbGreen
        
        /// If the image has a RGB color model, the buffer contains the blue channel.
        case rgbBlue
        
        ///  If the image has a XYZ color model, the buffer contains the X channel.
        case xyzX
        
        /// If the image has a XYZ color model, the buffer contains the Y channel.
        case xyzY
        
        /// If the image has a XYZ color model, the buffer contains the Z channel.
        case xyzZ
        
        public init?(rawValue: Int) {
            fatalError("Not supported, use `BufferType.init(bufferTypeCode:model:)`.")
        }
        
        /// Returns a new `BufferType` enum from the supplied code and color
        /// space model.
        public init?(bufferTypeCode: Int, model: CGColorSpaceModel?) {
            
            switch bufferTypeCode {
                
                //    kvImageBufferTypeCode_RGB_Red
                //    kvImageBufferTypeCode_CMYK_Cyan
                //    kvImageBufferTypeCode_LAB_L
                //    kvImageBufferTypeCode_XYZ_X
            //    kvImageBufferTypeCode_Monochrome
            case kvImageBufferTypeCode_ColorSpaceChannel1:
                switch model ?? .unknown {
                case .monochrome:
                    self = .monochrome
                case .cmyk:
                    self = .cmykCyan
                case .lab:
                    self = .labL
                case .XYZ:
                    self = .xyzX
                default:
                    self = .rgbRed
                }
                
                //    kvImageBufferTypeCode_RGB_Green
                //    kvImageBufferTypeCode_CMYK_Magenta
                //    kvImageBufferTypeCode_LAB_A
            //    kvImageBufferTypeCode_XYZ_Y
            case kvImageBufferTypeCode_ColorSpaceChannel2:
                switch model ?? .unknown {
                case .cmyk:
                    self = .cmykMagenta
                case .lab:
                    self = .labA
                case .XYZ:
                    self = .xyzY
                default:
                    self = .rgbGreen
                }
                
                //    kvImageBufferTypeCode_RGB_Blue
                //    kvImageBufferTypeCode_CMYK_Yellow
                //    kvImageBufferTypeCode_LAB_B
            //    kvImageBufferTypeCode_XYZ_Z
            case kvImageBufferTypeCode_ColorSpaceChannel3:
                switch model ?? .unknown {
                case .cmyk:
                    self = .cmykYellow
                case .lab:
                    self = .labB
                case .XYZ:
                    self = .xyzZ
                default:
                    self = .rgbBlue
                }
                
            case kvImageBufferTypeCode_Alpha:
                self = .alpha
            case kvImageBufferTypeCode_CGFormat:
                self = .coreGraphics
            case kvImageBufferTypeCode_CMYK_Black:
                self = .cmykBlack
            case kvImageBufferTypeCode_CVPixelBuffer_YCbCr:
                self = .YCbCr
            case kvImageBufferTypeCode_Cb:
                self = .Cb
            case kvImageBufferTypeCode_Cr:
                self = .Cr
            case kvImageBufferTypeCode_Chroma:
                self = .chroma
            case kvImageBufferTypeCode_Chunky:
                self = .chunky
            case kvImageBufferTypeCode_Indexed:
                self = .indexed
            case kvImageBufferTypeCode_Luminance:
                self = .luminance
                
            default:
                return nil
            }
        }
        
        /// The `vImageBufferTypeCode` for this `BufferType` enum.
        public var bufferTypeCode: vImageBufferTypeCode {
            var code = -1
            switch self {
            case .alpha:
                code = kvImageBufferTypeCode_Alpha
            case .coreGraphics:
                code = kvImageBufferTypeCode_CGFormat
            case .cmykBlack:
                code = kvImageBufferTypeCode_CMYK_Black
            case .cmykCyan:
                code = kvImageBufferTypeCode_CMYK_Cyan
            case .cmykMagenta:
                code = kvImageBufferTypeCode_CMYK_Magenta
            case .cmykYellow:
                code = kvImageBufferTypeCode_CMYK_Yellow
            case .YCbCr:
                code = kvImageBufferTypeCode_CVPixelBuffer_YCbCr
            case .Cb:
                code = kvImageBufferTypeCode_Cb
            case .Cr:
                code = kvImageBufferTypeCode_Cr
            case .chroma:
                code = kvImageBufferTypeCode_Chroma
            case .chunky:
                code = kvImageBufferTypeCode_Chunky
            case .indexed:
                code = kvImageBufferTypeCode_Indexed
            case .labA:
                code = kvImageBufferTypeCode_LAB_A
            case .labB:
                code = kvImageBufferTypeCode_LAB_B
            case .labL:
                code = kvImageBufferTypeCode_LAB_L
            case .luminance:
                code = kvImageBufferTypeCode_Luminance
            case .monochrome:
                code = kvImageBufferTypeCode_Monochrome
            case .rgbRed:
                code = kvImageBufferTypeCode_RGB_Red
            case .rgbGreen:
                code = kvImageBufferTypeCode_RGB_Green
            case .rgbBlue:
                code = kvImageBufferTypeCode_RGB_Blue
            case .xyzX:
                code = kvImageBufferTypeCode_XYZ_X
            case .xyzY:
                code = kvImageBufferTypeCode_XYZ_Y
            case .xyzZ:
                code = kvImageBufferTypeCode_XYZ_Z
            }
            
            return vImageBufferTypeCode(code)
        }
    }
}

@available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *)
extension vImageCVImageFormat {
    /// Core Video pixel format type enum.
    public enum Format {
        case format1Monochrome
        case format2Indexed
        case format4Indexed
        case format8Indexed
        case format1IndexedGray_WhiteIsZero
        case format2IndexedGray_WhiteIsZero
        case format4IndexedGray_WhiteIsZero
        case format8IndexedGray_WhiteIsZero
        case format16BE555
        case format16LE555
        case format16LE5551
        case format16BE565
        case format16LE565
        case format24RGB
        case format24BGR
        case format32ARGB
        case format32BGRA
        case format32ABGR
        case format32RGBA
        case format64ARGB
        case format48RGB
        case format32AlphaGray
        case format16Gray
        case format30RGB
        case format422YpCbCr8
        case format4444YpCbCrA8
        case format4444YpCbCrA8R
        case format4444AYpCbCr8
        case format4444AYpCbCr16
        case format444YpCbCr8
        case format422YpCbCr16
        case format422YpCbCr10
        case format444YpCbCr10
        case format420YpCbCr8Planar
        case format420YpCbCr8PlanarFullRange
        case format422YpCbCr_4A_8BiPlanar
        case format420YpCbCr8BiPlanarVideoRange
        case format420YpCbCr8BiPlanarFullRange
        case format422YpCbCr8_yuvs
        case format422YpCbCr8FullRange
        case formatOneComponent8
        case formatTwoComponent8
        case format30RGBLEPackedWideGamut
        case formatARGB2101010LEPacked
        case formatOneComponent16Half
        case formatOneComponent32Float
        case formatTwoComponent16Half
        case formatTwoComponent32Float
        case format64RGBAHalf
        case format128RGBAFloat
        case format14Bayer_GRBG
        case format14Bayer_RGGB
        case format14Bayer_BGGR
        case format14Bayer_GBRG
        case formatDisparityFloat16
        case formatDisparityFloat32
        case formatDepthFloat16
        case formatDepthFloat32
        case format420YpCbCr10BiPlanarVideoRange
        case format422YpCbCr10BiPlanarVideoRange
        case format444YpCbCr10BiPlanarVideoRange
        case format420YpCbCr10BiPlanarFullRange
        case format422YpCbCr10BiPlanarFullRange
        case format444YpCbCr10BiPlanarFullRange
        
        var ostype: OSType {
            switch self {
            case .format1Monochrome:
                return kCVPixelFormatType_1Monochrome
            case .format2Indexed:
                return kCVPixelFormatType_2Indexed
            case .format4Indexed:
                return kCVPixelFormatType_4Indexed
            case .format8Indexed:
                return kCVPixelFormatType_8Indexed
            case .format1IndexedGray_WhiteIsZero:
                return kCVPixelFormatType_1IndexedGray_WhiteIsZero
            case .format2IndexedGray_WhiteIsZero:
                return kCVPixelFormatType_2IndexedGray_WhiteIsZero
            case .format4IndexedGray_WhiteIsZero:
                return kCVPixelFormatType_4IndexedGray_WhiteIsZero
            case .format8IndexedGray_WhiteIsZero:
                return kCVPixelFormatType_8IndexedGray_WhiteIsZero
            case .format16BE555:
                return kCVPixelFormatType_16BE555
            case .format16LE555:
                return kCVPixelFormatType_16LE555
            case .format16LE5551:
                return kCVPixelFormatType_16LE5551
            case .format16BE565:
                return kCVPixelFormatType_16BE565
            case .format16LE565:
                return kCVPixelFormatType_16LE565
            case .format24RGB:
                return kCVPixelFormatType_24RGB
            case .format24BGR:
                return kCVPixelFormatType_24BGR
            case .format32ARGB:
                return kCVPixelFormatType_32ARGB
            case .format32BGRA:
                return kCVPixelFormatType_32BGRA
            case .format32ABGR:
                return kCVPixelFormatType_32ABGR
            case .format32RGBA:
                return kCVPixelFormatType_32RGBA
            case .format64ARGB:
                return kCVPixelFormatType_64ARGB
            case .format48RGB:
                return kCVPixelFormatType_48RGB
            case .format32AlphaGray:
                return kCVPixelFormatType_32AlphaGray
            case .format16Gray:
                return kCVPixelFormatType_16Gray
            case .format30RGB:
                return kCVPixelFormatType_30RGB
            case .format422YpCbCr8:
                return kCVPixelFormatType_422YpCbCr8
            case .format4444YpCbCrA8:
                return kCVPixelFormatType_4444YpCbCrA8
            case .format4444YpCbCrA8R:
                return kCVPixelFormatType_4444YpCbCrA8R
            case .format4444AYpCbCr8:
                return kCVPixelFormatType_4444AYpCbCr8
            case .format4444AYpCbCr16:
                return kCVPixelFormatType_4444AYpCbCr16
            case .format444YpCbCr8:
                return kCVPixelFormatType_444YpCbCr8
            case .format422YpCbCr16:
                return kCVPixelFormatType_422YpCbCr16
            case .format422YpCbCr10:
                return kCVPixelFormatType_422YpCbCr10
            case .format444YpCbCr10:
                return kCVPixelFormatType_444YpCbCr10
            case .format420YpCbCr8Planar:
                return kCVPixelFormatType_420YpCbCr8Planar
            case .format420YpCbCr8PlanarFullRange:
                return kCVPixelFormatType_420YpCbCr8PlanarFullRange
            case .format422YpCbCr_4A_8BiPlanar:
                return kCVPixelFormatType_422YpCbCr_4A_8BiPlanar
            case .format420YpCbCr8BiPlanarVideoRange:
                return kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange
            case .format420YpCbCr8BiPlanarFullRange:
                return kCVPixelFormatType_420YpCbCr8BiPlanarFullRange
            case .format422YpCbCr8_yuvs:
                return kCVPixelFormatType_422YpCbCr8_yuvs
            case .format422YpCbCr8FullRange:
                return kCVPixelFormatType_422YpCbCr8FullRange
            case .formatOneComponent8:
                return kCVPixelFormatType_OneComponent8
            case .formatTwoComponent8:
                return kCVPixelFormatType_TwoComponent8
            case .format30RGBLEPackedWideGamut:
                return kCVPixelFormatType_30RGBLEPackedWideGamut
            case .formatARGB2101010LEPacked:
                return kCVPixelFormatType_ARGB2101010LEPacked
            case .formatOneComponent16Half:
                return kCVPixelFormatType_OneComponent16Half
            case .formatOneComponent32Float:
                return kCVPixelFormatType_OneComponent32Float
            case .formatTwoComponent16Half:
                return kCVPixelFormatType_TwoComponent16Half
            case .formatTwoComponent32Float:
                return kCVPixelFormatType_TwoComponent32Float
            case .format64RGBAHalf:
                return kCVPixelFormatType_64RGBAHalf
            case .format128RGBAFloat:
                return kCVPixelFormatType_128RGBAFloat
            case .format14Bayer_GRBG:
                return kCVPixelFormatType_14Bayer_GRBG
            case .format14Bayer_RGGB:
                return kCVPixelFormatType_14Bayer_RGGB
            case .format14Bayer_BGGR:
                return kCVPixelFormatType_14Bayer_BGGR
            case .format14Bayer_GBRG:
                return kCVPixelFormatType_14Bayer_GBRG
            case .formatDisparityFloat16:
                return kCVPixelFormatType_DisparityFloat16
            case .formatDisparityFloat32:
                return kCVPixelFormatType_DisparityFloat32
            case .formatDepthFloat16:
                return kCVPixelFormatType_DepthFloat16
            case .formatDepthFloat32:
                return kCVPixelFormatType_DepthFloat32
            case .format420YpCbCr10BiPlanarVideoRange:
                return kCVPixelFormatType_420YpCbCr10BiPlanarVideoRange
            case .format422YpCbCr10BiPlanarVideoRange:
                return kCVPixelFormatType_422YpCbCr10BiPlanarVideoRange
            case .format444YpCbCr10BiPlanarVideoRange:
                return kCVPixelFormatType_444YpCbCr10BiPlanarVideoRange
            case .format420YpCbCr10BiPlanarFullRange:
                return kCVPixelFormatType_420YpCbCr10BiPlanarFullRange
            case .format422YpCbCr10BiPlanarFullRange:
                return kCVPixelFormatType_422YpCbCr10BiPlanarFullRange
            case .format444YpCbCr10BiPlanarFullRange:
                return kCVPixelFormatType_444YpCbCr10BiPlanarFullRange
            }
        }
    }
    
    public enum ChromaSiting {
        /// Chroma sample is horizontally co-sited with the left column of luma
        /// samples, but centered vertically.
        case left
        
        /// Chroma sample is fully centered
        case center
        
        /// Chroma sample is co-sited with the top-left luma sample.
        case topLeft
        
        /// Chroma sample is horizontally centered, but co-sited with the top
        /// row of luma samples.
        case top
        
        /// Chroma sample is co-sited with the bottom-left luma sample.
        case bottomLeft
        
        /// Chroma sample is horizontally centered, but co-sited with the
        /// bottom row of luma samples.
        case bottom
        
        /// Cr and Cb samples are alternately co-sited with the left luma
        /// samples of the same field.
        case dv420
        
        /// Returns a new `ChromaSiting` enum from the supplied location.
        init?(location: CFString?) {
            switch location {
            case kCVImageBufferChromaLocation_Left:
                self = .left
            case kCVImageBufferChromaLocation_Center:
                self = .center
            case kCVImageBufferChromaLocation_TopLeft:
                self = .topLeft
            case kCVImageBufferChromaLocation_Top:
                self = .top
            case kCVImageBufferChromaLocation_BottomLeft:
                self = .bottomLeft
            case kCVImageBufferChromaLocation_Bottom:
                self = .bottom
            case kCVImageBufferChromaLocation_DV420:
                self = .dv420
            default:
                return nil
            }
        }
        
        /// Returns a `CFString` describing this `ChromaSiting`.
        var cfString: CFString {
            switch self {
            case .left:
                return kCVImageBufferChromaLocation_Left
            case .center:
                return kCVImageBufferChromaLocation_Center
            case .topLeft:
                return kCVImageBufferChromaLocation_TopLeft
            case .top:
                return kCVImageBufferChromaLocation_Top
            case .bottomLeft:
                return kCVImageBufferChromaLocation_BottomLeft
            case .bottom:
                return kCVImageBufferChromaLocation_Bottom
            case .dv420:
                return kCVImageBufferChromaLocation_DV420
            }
        }
    }
}
