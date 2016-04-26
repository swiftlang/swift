
@available(tvOS 4.0, *)
let kCVPixelFormatName: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatConstant: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatCodecType: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatFourCC: CFString
@available(tvOS 4.3, *)
let kCVPixelFormatContainsAlpha: CFString
@available(tvOS 8.0, *)
let kCVPixelFormatContainsYCbCr: CFString
@available(tvOS 8.0, *)
let kCVPixelFormatContainsRGB: CFString
@available(tvOS 9.0, *)
let kCVPixelFormatComponentRange: CFString
@available(tvOS 9.0, *)
let kCVPixelFormatComponentRange_VideoRange: CFString
@available(tvOS 9.0, *)
let kCVPixelFormatComponentRange_FullRange: CFString
@available(tvOS 9.0, *)
let kCVPixelFormatComponentRange_WideRange: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatPlanes: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBlockWidth: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBlockHeight: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBitsPerBlock: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBlockHorizontalAlignment: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBlockVerticalAlignment: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatBlackBlock: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatHorizontalSubsampling: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatVerticalSubsampling: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatOpenGLFormat: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatOpenGLType: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatOpenGLInternalFormat: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatCGBitmapInfo: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatQDCompatibility: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatCGBitmapContextCompatibility: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatCGImageCompatibility: CFString
@available(tvOS 4.0, *)
let kCVPixelFormatOpenGLCompatibility: CFString
@available(tvOS 5.0, *)
let kCVPixelFormatOpenGLESCompatibility: CFString
typealias CVFillExtendedPixelsCallBack = @convention(c) (CVPixelBuffer, UnsafeMutablePointer<Void>?) -> DarwinBoolean
struct CVFillExtendedPixelsCallBackData {
  var version: CFIndex
  var fillCallBack: CVFillExtendedPixelsCallBack?
  var refCon: UnsafeMutablePointer<Void>?
  init()
  init(version version: CFIndex, fillCallBack fillCallBack: CVFillExtendedPixelsCallBack?, refCon refCon: UnsafeMutablePointer<Void>?)
}
@available(tvOS 4.0, *)
let kCVPixelFormatFillExtendedPixelsCallback: CFString
@available(tvOS 4.0, *)
@discardableResult
func CVPixelFormatDescriptionCreateWithPixelFormatType(_ allocator: CFAllocator?, _ pixelFormat: OSType) -> Unmanaged<CFDictionary>?
@available(tvOS 4.0, *)
@discardableResult
func CVPixelFormatDescriptionArrayCreateWithAllPixelFormatTypes(_ allocator: CFAllocator?) -> Unmanaged<CFArray>?
@available(tvOS 4.0, *)
func CVPixelFormatDescriptionRegisterDescriptionWithPixelFormatType(_ description: CFDictionary, _ pixelFormat: OSType)
