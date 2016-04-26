
@available(OSX 10.4, *)
let kCVPixelFormatName: CFString
@available(OSX 10.4, *)
let kCVPixelFormatConstant: CFString
@available(OSX 10.4, *)
let kCVPixelFormatCodecType: CFString
@available(OSX 10.4, *)
let kCVPixelFormatFourCC: CFString
@available(OSX 10.7, *)
let kCVPixelFormatContainsAlpha: CFString
@available(OSX 10.10, *)
let kCVPixelFormatContainsYCbCr: CFString
@available(OSX 10.10, *)
let kCVPixelFormatContainsRGB: CFString
@available(OSX 10.10, *)
let kCVPixelFormatComponentRange: CFString
@available(OSX 10.10, *)
let kCVPixelFormatComponentRange_VideoRange: CFString
@available(OSX 10.10, *)
let kCVPixelFormatComponentRange_FullRange: CFString
@available(OSX 10.10, *)
let kCVPixelFormatComponentRange_WideRange: CFString
@available(OSX 10.4, *)
let kCVPixelFormatPlanes: CFString
@available(OSX 10.4, *)
let kCVPixelFormatBlockWidth: CFString
@available(OSX 10.4, *)
let kCVPixelFormatBlockHeight: CFString
@available(OSX 10.4, *)
let kCVPixelFormatBitsPerBlock: CFString
@available(OSX 10.4, *)
let kCVPixelFormatBlockHorizontalAlignment: CFString
@available(OSX 10.4, *)
let kCVPixelFormatBlockVerticalAlignment: CFString
@available(OSX 10.6, *)
let kCVPixelFormatBlackBlock: CFString
@available(OSX 10.4, *)
let kCVPixelFormatHorizontalSubsampling: CFString
@available(OSX 10.4, *)
let kCVPixelFormatVerticalSubsampling: CFString
@available(OSX 10.4, *)
let kCVPixelFormatOpenGLFormat: CFString
@available(OSX 10.4, *)
let kCVPixelFormatOpenGLType: CFString
@available(OSX 10.4, *)
let kCVPixelFormatOpenGLInternalFormat: CFString
@available(OSX 10.4, *)
let kCVPixelFormatCGBitmapInfo: CFString
@available(OSX 10.4, *)
let kCVPixelFormatQDCompatibility: CFString
@available(OSX 10.4, *)
let kCVPixelFormatCGBitmapContextCompatibility: CFString
@available(OSX 10.4, *)
let kCVPixelFormatCGImageCompatibility: CFString
@available(OSX 10.4, *)
let kCVPixelFormatOpenGLCompatibility: CFString
typealias CVFillExtendedPixelsCallBack = @convention(c) (CVPixelBuffer, UnsafeMutablePointer<Void>?) -> DarwinBoolean
struct CVFillExtendedPixelsCallBackData {
  var version: CFIndex
  var fillCallBack: CVFillExtendedPixelsCallBack?
  var refCon: UnsafeMutablePointer<Void>?
  init()
  init(version version: CFIndex, fillCallBack fillCallBack: CVFillExtendedPixelsCallBack?, refCon refCon: UnsafeMutablePointer<Void>?)
}
@available(OSX 10.4, *)
let kCVPixelFormatFillExtendedPixelsCallback: CFString
@available(OSX 10.4, *)
@discardableResult
func CVPixelFormatDescriptionCreateWithPixelFormatType(_ allocator: CFAllocator?, _ pixelFormat: OSType) -> Unmanaged<CFDictionary>?
@available(OSX 10.4, *)
@discardableResult
func CVPixelFormatDescriptionArrayCreateWithAllPixelFormatTypes(_ allocator: CFAllocator?) -> Unmanaged<CFArray>?
@available(OSX 10.4, *)
func CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType(_ description: CFDictionary, _ pixelFormat: OSType)
