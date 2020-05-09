// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: rdar50244151
// REQUIRES: objc_interop
// UNSUPPORTED: OS=watchos

import StdlibUnittest
import Accelerate

var Accelerate_vImageTests = TestSuite("Accelerate_vImage")

if #available(macOS 10.15, iOS 13.0, tvOS 13.0, watchOS 6.0, *) {
    let width = UInt(64)
    let height = UInt(32)
    let widthi = 64
    let heighti = 32
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: Converter
    //
    //===----------------------------------------------------------------------===//
    
    Accelerate_vImageTests.test("vImage/CVConverters") {
        let colorSpace = CGColorSpaceCreateDeviceRGB()
        
        let cgFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                            bitsPerPixel: 32,
                                            colorSpace: colorSpace,
                                            bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.first.rawValue),
                                            renderingIntent: .defaultIntent)!
        
        let cvFormat = vImageCVImageFormat.make(format: .format32ABGR,
                                                matrix: kvImage_ARGBToYpCbCrMatrix_ITU_R_601_4.pointee,
                                                chromaSiting: .center,
                                                colorSpace: colorSpace,
                                                alphaIsOpaqueHint: false)!
        
        let coreVideoToCoreGraphics = try! vImageConverter.make(sourceFormat: cvFormat,
                                                                destinationFormat: cgFormat,
                                                                flags: .printDiagnosticsToConsole)
        
        let coreGraphicsToCoreVideo = try! vImageConverter.make(sourceFormat: cgFormat,
                                                                destinationFormat: cvFormat,
                                                                flags: .printDiagnosticsToConsole)
        
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        let sourceCGBuffer = try! vImage_Buffer(cgImage: image)
        var intermediateCVBuffer = try! vImage_Buffer(width: widthi, height: heighti, bitsPerPixel: 32)
        var destinationCGBuffer = try! vImage_Buffer(width: widthi, height: heighti, bitsPerPixel: 32)
        
        try! coreGraphicsToCoreVideo.convert(source: sourceCGBuffer,
                                             destination: &intermediateCVBuffer)
        
        try! coreVideoToCoreGraphics.convert(source: intermediateCVBuffer,
                                             destination: &destinationCGBuffer)
        
        let destinationPixels: [UInt8] = arrayFromBuffer(buffer: destinationCGBuffer,
                                                         channelCount: 4,
                                                         count: pixels.count)
        
        expectEqual(destinationPixels, pixels)
        
        expectEqual(coreVideoToCoreGraphics.destinationBuffers(colorSpace: colorSpace),
                    coreGraphicsToCoreVideo.sourceBuffers(colorSpace: colorSpace))
        
        expectEqual(coreVideoToCoreGraphics.sourceBuffers(colorSpace: colorSpace),
                    coreGraphicsToCoreVideo.destinationBuffers(colorSpace: colorSpace))
    }
  
    /* Disabled due to <rdar://problem/50209312>
    Accelerate_vImageTests.test("vImage/BufferOrder") {
        let sourceFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                                bitsPerPixel: 32,
                                                colorSpace: CGColorSpaceCreateDeviceCMYK(),
                                                bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue),
                                                renderingIntent: .defaultIntent)!
        
        let destinationFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                                     bitsPerPixel: 24,
                                                     colorSpace: CGColorSpace(name: CGColorSpace.genericLab)!,
                                                     bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue),
                                                     renderingIntent: .defaultIntent)!
        
        let converter = try! vImageConverter.make(sourceFormat: sourceFormat,
                                                  destinationFormat: destinationFormat)
        
        let sBuffers = converter.sourceBuffers(colorSpace: sourceFormat.colorSpace.takeRetainedValue())
        let dBuffers = converter.destinationBuffers(colorSpace: destinationFormat.colorSpace.takeRetainedValue())
        
        expectEqual(sBuffers, [.coreGraphics])
        expectEqual(dBuffers, [.coreGraphics])
    }
    */
    
    Accelerate_vImageTests.test("vImage/AnyToAnyError") {
        var error = kvImageNoError
        
        let format = vImage_CGImageFormat(bitsPerComponent: 8,
                                          bitsPerPixel: 32,
                                          colorSpace: CGColorSpaceCreateDeviceCMYK(),
                                          bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue),
                                          renderingIntent: .defaultIntent)!
        
        expectCrashLater()
        _ = try! vImageConverter.make(sourceFormat: format,
                                      destinationFormat: format,
                                      flags: .imageExtend)
    }
    
    Accelerate_vImageTests.test("vImage/AnyToAny") {
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        let sourceFormat = vImage_CGImageFormat(cgImage: image)!
        
        let destinationFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                                     bitsPerPixel: 32,
                                                     colorSpace: CGColorSpaceCreateDeviceCMYK(),
                                                     bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.none.rawValue),
                                                     renderingIntent: .defaultIntent)!
        
        let sourceBuffer = try! vImage_Buffer(cgImage: image)
        var destinationBuffer = try! vImage_Buffer(width: widthi, height: heighti,
                                                   bitsPerPixel: 32)
        
        // New API
        
        let converter = try! vImageConverter.make(sourceFormat: sourceFormat,
                                                  destinationFormat: destinationFormat)
        
        try! converter.convert(source: sourceBuffer,
                               destination: &destinationBuffer)
        
        let mustOperateOutOfPlace = try! converter.mustOperateOutOfPlace(source: sourceBuffer,
                                                                         destination: destinationBuffer)
        
        // Legacy API
        
        var legacyDestinationBuffer = try! vImage_Buffer(width: widthi, height: heighti,
                                                         bitsPerPixel: 32)
        
        var legacyConverter: vImageConverter?
        
        withUnsafePointer(to: destinationFormat) { dest in
            withUnsafePointer(to: sourceFormat) { src in
                legacyConverter = vImageConverter_CreateWithCGImageFormat(
                    src,
                    dest,
                    nil,
                    vImage_Flags(kvImageNoFlags),
                    nil)?.takeRetainedValue()
            }
        }
        
        _ = withUnsafePointer(to: sourceBuffer) { src in
            vImageConvert_AnyToAny(legacyConverter!,
                                   src,
                                   &legacyDestinationBuffer,
                                   nil,
                                   vImage_Flags(kvImageNoFlags))
        }
        
        var e = kvImageNoError
        withUnsafePointer(to: sourceBuffer) { src in
            withUnsafePointer(to: destinationBuffer) { dest in
                e = vImageConverter_MustOperateOutOfPlace(legacyConverter!,
                                                          src,
                                                          dest,
                                                          vImage_Flags(kvImageNoFlags))
            }
        }
        let legacyMustOperateOutOfPlace = e == kvImageOutOfPlaceOperationRequired
        
        // Compare results
        
        let destinationPixels: [UInt8] = arrayFromBuffer(buffer: destinationBuffer,
                                                         channelCount: 4,
                                                         count: pixels.count)
        let legacyDestinationPixels: [UInt8] = arrayFromBuffer(buffer: legacyDestinationBuffer,
                                                               channelCount: 4,
                                                               count: pixels.count)
        
        expectTrue(legacyDestinationPixels.elementsEqual(destinationPixels))
        
        expectEqual(converter.sourceBufferCount, 1)
        expectEqual(converter.destinationBufferCount, 1)
        expectEqual(legacyMustOperateOutOfPlace, mustOperateOutOfPlace)
        
        sourceBuffer.free()
        destinationBuffer.free()
        legacyDestinationBuffer.free()
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: Buffer
    //
    //===----------------------------------------------------------------------===//
    
    Accelerate_vImageTests.test("vImage/IllegalSize") {
        expectCrashLater()
        let buffer = try! vImage_Buffer(width: -1, height: -1,
                                        bitsPerPixel: 32)
    }
    
    Accelerate_vImageTests.test("vImage/IllegalSize") {
        expectCrashLater()
        let buffer = try! vImage_Buffer(width: 99999999, height: 99999999,
                                        bitsPerPixel: 99999999)
    }
    
    Accelerate_vImageTests.test("vImage/InitWithInvalidImageFormat") {
        let pixels: [UInt8] = (0 ..< width * height).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makePlanar8Image(from: pixels,
                                     width: width,
                                     height: height)!
        
        let format = vImage_CGImageFormat(bitsPerComponent: 8,
                                          bitsPerPixel: 32,
                                          colorSpace: CGColorSpace(name: CGColorSpace.sRGB)!,
                                          bitmapInfo: CGBitmapInfo(rawValue: 0))!
        
        var error = kvImageNoError
        
        expectCrashLater()
        let buffer = try! vImage_Buffer(cgImage: image,
                                        format: format)
    }
    
    Accelerate_vImageTests.test("vImage/CreateCGImage") {
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        let buffer = try! vImage_Buffer(cgImage: image)
        
        let format = vImage_CGImageFormat(cgImage: image)!
        
        let bufferImage = try! buffer.createCGImage(format: format)
        
        let imagePixels = imageToPixels(image: image)
        let bufferImagePixels = imageToPixels(image: bufferImage)
        
        expectTrue(imagePixels.elementsEqual(bufferImagePixels))
        
        buffer.free()
    }
    
    Accelerate_vImageTests.test("vImage/CopyBadWidth") {
        var source = try! vImage_Buffer(width: 10, height: 100, bitsPerPixel: 32)
        var destination = try! vImage_Buffer(width: 20, height: 20, bitsPerPixel: 32)
        
        expectCrashLater()
        try! source.copy(destinationBuffer: &destination,
                         pixelSize: 4)
    }

    Accelerate_vImageTests.test("vImage/CopyBadHeight") {
        var source = try! vImage_Buffer(width: 100, height: 10, bitsPerPixel: 32)
        var destination = try! vImage_Buffer(width: 20, height: 20, bitsPerPixel: 32)
        
        expectCrashLater()
        try! source.copy(destinationBuffer: &destination,
                         pixelSize: 4)
    }
    
    Accelerate_vImageTests.test("vImage/Copy") {
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        let source = try! vImage_Buffer(cgImage: image)
        
        var destination = try! vImage_Buffer(width: widthi, height: heighti,
                                             bitsPerPixel: 32)
        
        try! source.copy(destinationBuffer: &destination,
                         pixelSize: 4)
        
        let sourcePixels: [UInt8] = arrayFromBuffer(buffer: source,
                                                    channelCount: 4,
                                                    count: pixels.count)
        let destinationPixels: [UInt8] = arrayFromBuffer(buffer: destination,
                                                         channelCount: 4,
                                                         count: pixels.count)
        
        expectTrue(sourcePixels.elementsEqual(destinationPixels))
        
        source.free()
        destination.free()
    }

    Accelerate_vImageTests.test("vImage/InitializeWithFormat") {
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        let format = vImage_CGImageFormat(cgImage: image)!
        
        let buffer = try! vImage_Buffer(cgImage: image,
                                        format:  format)
        
        let bufferPixels: [UInt8] = arrayFromBuffer(buffer: buffer,
                                                    channelCount: 4,
                                                    count: pixels.count)
        
        expectTrue(bufferPixels.elementsEqual(pixels))
        expectEqual(buffer.rowBytes, Int(width) * 4)
        expectEqual(buffer.width, width)
        expectEqual(buffer.height, height)
        
        buffer.free()
    }
    
    Accelerate_vImageTests.test("vImage/Alignment") {
        // New API
        
        var alignment = try! vImage_Buffer.preferredAlignmentAndRowBytes(width: widthi,
                                                                         height: heighti,
                                                                         bitsPerPixel: 32)
        
        // Legacy API
        
        var legacyBuffer = vImage_Buffer()
        let legacyAlignment = vImageBuffer_Init(&legacyBuffer,
                                                height, width,
                                                32,
                                                vImage_Flags(kvImageNoAllocate))
        
        expectEqual(alignment.alignment, legacyAlignment)
        expectEqual(alignment.rowBytes, legacyBuffer.rowBytes)
        
        legacyBuffer.free()
    }
    
    Accelerate_vImageTests.test("vImage/CreateBufferFromCGImage") {
        let pixels: [UInt8] = (0 ..< width * height * 4).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makeRGBA8888Image(from: pixels,
                                      width: width,
                                      height: height)!
        
        // New API
        
        let buffer = try! vImage_Buffer(cgImage: image)
        
        // Legacy API
        
        let legacyBuffer: vImage_Buffer = {
            var format = vImage_CGImageFormat(cgImage: image)!
            
            var buffer = vImage_Buffer()
            
            vImageBuffer_InitWithCGImage(&buffer,
                                         &format,
                                         nil,
                                         image,
                                         vImage_Flags(kvImageNoFlags))
            
            return buffer
        }()
        
        let bufferPixels: [UInt8] = arrayFromBuffer(buffer: buffer,
                                                    channelCount: 4,
                                                    count: pixels.count)
        
        let legacyBufferPixels: [UInt8] = arrayFromBuffer(buffer: legacyBuffer,
                                                          channelCount: 4,
                                                          count: pixels.count)
        
        expectTrue(bufferPixels.elementsEqual(legacyBufferPixels))
        
        expectEqual(buffer.width, legacyBuffer.width)
        expectEqual(buffer.height, legacyBuffer.height)
        expectEqual(buffer.rowBytes, legacyBuffer.rowBytes)
        
        expectEqual(buffer.size, CGSize(width: Int(width),
                                        height: Int(height)))
        
        buffer.free()
        free(legacyBuffer.data)
    }
    
    //===----------------------------------------------------------------------===//
    //
    //  MARK: CVImageFormat
    //
    //===----------------------------------------------------------------------===//
    
    Accelerate_vImageTests.test("vImage/MakeFromPixelBuffer") {
        let pixelBuffer = makePixelBuffer(pixelFormat: kCVPixelFormatType_420YpCbCr8Planar)!
        
        let format = vImageCVImageFormat.make(buffer: pixelBuffer)!
        
        expectEqual(format.channels, [vImage.BufferType.luminance,
                                      vImage.BufferType.Cb,
                                      vImage.BufferType.Cr])
    }
    
    Accelerate_vImageTests.test("vImage/MakeFormat4444YpCbCrA8") {
        let format = vImageCVImageFormat.make(format: .format4444YpCbCrA8,
                                              matrix: kvImage_ARGBToYpCbCrMatrix_ITU_R_709_2.pointee,
                                              chromaSiting: .center,
                                              colorSpace: CGColorSpaceCreateDeviceRGB(),
                                              alphaIsOpaqueHint: false)!
        
        // test alphaIsOpaqueHint
        
        expectEqual(format.alphaIsOpaqueHint, false)
        
        format.alphaIsOpaqueHint = true
        
        expectEqual(format.alphaIsOpaqueHint, true)
        
        // test colorSpace
        
        expectEqual(String(format.colorSpace!.name!), "kCGColorSpaceDeviceRGB")
        
        format.colorSpace = CGColorSpace(name: CGColorSpace.extendedLinearSRGB)!
        
        expectEqual(String(format.colorSpace!.name!), "kCGColorSpaceExtendedLinearSRGB")
        
        // test channel names
        
        let channels = format.channels
        
        expectEqual(channels,
                    [vImage.BufferType.Cb,
                     vImage.BufferType.luminance,
                     vImage.BufferType.Cr,
                     vImage.BufferType.alpha])
        
        let description = format.channelDescription(bufferType: channels.first!)
        
        expectEqual(description?.min, 0)
        expectEqual(description?.max, 255)
        expectEqual(description?.full, 240)
        expectEqual(description?.zero, 128)
    }
    
    Accelerate_vImageTests.test("vImage/MakeFormat420YpCbCr8Planar") {
        let format = vImageCVImageFormat.make(format: .format420YpCbCr8Planar,
                                              matrix: kvImage_ARGBToYpCbCrMatrix_ITU_R_709_2.pointee,
                                              chromaSiting: .center,
                                              colorSpace: CGColorSpaceCreateDeviceRGB(),
                                              alphaIsOpaqueHint: false)!
        
        // test chromaSiting
        
        expectEqual(format.chromaSiting, vImageCVImageFormat.ChromaSiting.center)
        
        format.chromaSiting = .dv420
        
        expectEqual(format.chromaSiting, vImageCVImageFormat.ChromaSiting.dv420)
        
        // test formatCode
        
        expectEqual(format.formatCode, kCVPixelFormatType_420YpCbCr8Planar)
        
        // test channelCount
        
        expectEqual(format.channelCount, 3)
    }

    //===----------------------------------------------------------------------===//
    //
    //  MARK: CGImageFormat
    //
    //===----------------------------------------------------------------------===//
    
    Accelerate_vImageTests.test("vImage/CGImageFormatIllegalValues") {
        
        let formatOne = vImage_CGImageFormat(bitsPerComponent: -1,
                                             bitsPerPixel: 32,
                                             colorSpace: CGColorSpaceCreateDeviceRGB(),
                                             bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue))
        
        let formatTwo = vImage_CGImageFormat(bitsPerComponent: 8,
                                             bitsPerPixel: -1,
                                             colorSpace: CGColorSpaceCreateDeviceRGB(),
                                             bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue))
        
        expectTrue(formatOne == nil)
        expectTrue(formatTwo == nil)
    }
    
    Accelerate_vImageTests.test("vImage/CGImageFormatFromCGImage") {
        let pixels: [UInt8] = (0 ..< width * height).map { _ in
            return UInt8.random(in: 0 ..< 255)
        }
        
        let image = makePlanar8Image(from: pixels,
                                     width: width,
                                     height: height)!
        
        var format = vImage_CGImageFormat(cgImage: image)!
        
        var legacyFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                                bitsPerPixel: 8,
                                                colorSpace: Unmanaged.passRetained(CGColorSpaceCreateDeviceGray()),
                                                bitmapInfo: CGBitmapInfo(rawValue: 0),
                                                version: 0,
                                                decode: nil,
                                                renderingIntent: .defaultIntent)
        
        expectTrue(vImageCGImageFormat_IsEqual(&format, &legacyFormat))
    }
    
    Accelerate_vImageTests.test("vImage/CGImageFormatLightweightInit") {
        let colorspace = CGColorSpaceCreateDeviceRGB()
        let renderingIntent = CGColorRenderingIntent.defaultIntent
        let bitmapInfo = CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue)
        
        var format = vImage_CGImageFormat(bitsPerComponent: 8,
                                          bitsPerPixel: 32,
                                          colorSpace: colorspace,
                                          bitmapInfo: bitmapInfo)!
        
        var legacyFormat = vImage_CGImageFormat(bitsPerComponent: 8,
                                                bitsPerPixel: 32,
                                                colorSpace: Unmanaged.passRetained(colorspace),
                                                bitmapInfo: bitmapInfo,
                                                version: 0,
                                                decode: nil,
                                                renderingIntent: renderingIntent)
        
        expectTrue(vImageCGImageFormat_IsEqual(&format, &legacyFormat))
        expectTrue(format.componentCount == 4)
    }

    //===----------------------------------------------------------------------===//
    //
    //  MARK: Helper Functions
    //
    //===----------------------------------------------------------------------===//
    
    func makePixelBuffer(pixelFormat: OSType) -> CVPixelBuffer? {
        var pixelBuffer: CVPixelBuffer? = nil
        CVPixelBufferCreate(kCFAllocatorDefault,
                            1,
                            1,
                            pixelFormat,
                            [:] as CFDictionary?,
                            &pixelBuffer)
        
        return pixelBuffer
    }
    
    func arrayFromBuffer<T>(buffer: vImage_Buffer,
                            channelCount: Int,
                            count: Int) -> Array<T> {
        
        if (buffer.rowBytes == Int(buffer.width) * MemoryLayout<T>.stride * channelCount) {
            let ptr = buffer.data.bindMemory(to: T.self,
                                             capacity: count)
            
            let buf = UnsafeBufferPointer(start: ptr, count: count)
            return Array(buf)
        } else {
            var returnArray = [T]()
            
            let perRowCount = Int(buffer.width) * MemoryLayout<T>.stride * channelCount
            var ptr = buffer.data.bindMemory(to: T.self,
                                             capacity: perRowCount)
            
            for _ in 0 ..< buffer.height {
                let buf = UnsafeBufferPointer(start: ptr, count: perRowCount)
                
                returnArray.append(contentsOf: Array(buf))
                
                ptr = ptr.advanced(by: buffer.rowBytes)
            }
            
            return returnArray
        }
    }
    
    func imageToPixels(image: CGImage) -> [UInt8] {
        let pixelCount = image.width * image.height
        
        let pixelData = image.dataProvider?.data
        let pixels: UnsafePointer<UInt8> = CFDataGetBytePtr(pixelData)
        let buf = UnsafeBufferPointer(start: pixels, count: pixelCount)
        return Array(buf)
    }
    
    func makePlanar8Image(from pixels: [UInt8],
                          width: UInt,
                          height: UInt) -> CGImage? {
        
        if
            let outputData = CFDataCreate(nil, pixels, pixels.count),
            let cgDataProvider = CGDataProvider(data: outputData) {
            
            return CGImage(width: Int(width),
                           height: Int(height),
                           bitsPerComponent: 8,
                           bitsPerPixel: 8,
                           bytesPerRow: Int(width),
                           space: CGColorSpaceCreateDeviceGray(),
                           bitmapInfo: CGBitmapInfo(rawValue: 0),
                           provider: cgDataProvider,
                           decode: nil,
                           shouldInterpolate: false,
                           intent: CGColorRenderingIntent.defaultIntent)
            
        }
        return nil
    }
    
    func makeRGBA8888Image(from pixels: [UInt8],
                           width: UInt,
                           height: UInt) -> CGImage? {
        
        if
            let outputData = CFDataCreate(nil, pixels, pixels.count),
            let cgDataProvider = CGDataProvider(data: outputData) {
            
            return CGImage(width: Int(width),
                           height: Int(height),
                           bitsPerComponent: 8,
                           bitsPerPixel: 8 * 4,
                           bytesPerRow: Int(width) * 4,
                           space: CGColorSpace(name: CGColorSpace.sRGB)!,
                           bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.last.rawValue),
                           provider: cgDataProvider,
                           decode: nil,
                           shouldInterpolate: false,
                           intent: CGColorRenderingIntent.defaultIntent)
            
        }
        return nil
    }
}
runAllTests()
