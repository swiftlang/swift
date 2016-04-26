
var kCVPixelFormatType_1Monochrome: OSType { get }
var kCVPixelFormatType_2Indexed: OSType { get }
var kCVPixelFormatType_4Indexed: OSType { get }
var kCVPixelFormatType_8Indexed: OSType { get }
var kCVPixelFormatType_1IndexedGray_WhiteIsZero: OSType { get }
var kCVPixelFormatType_2IndexedGray_WhiteIsZero: OSType { get }
var kCVPixelFormatType_4IndexedGray_WhiteIsZero: OSType { get }
var kCVPixelFormatType_8IndexedGray_WhiteIsZero: OSType { get }
var kCVPixelFormatType_16BE555: OSType { get }
var kCVPixelFormatType_16LE555: OSType { get }
var kCVPixelFormatType_16LE5551: OSType { get }
var kCVPixelFormatType_16BE565: OSType { get }
var kCVPixelFormatType_16LE565: OSType { get }
var kCVPixelFormatType_24RGB: OSType { get }
var kCVPixelFormatType_24BGR: OSType { get }
var kCVPixelFormatType_32ARGB: OSType { get }
var kCVPixelFormatType_32BGRA: OSType { get }
var kCVPixelFormatType_32ABGR: OSType { get }
var kCVPixelFormatType_32RGBA: OSType { get }
var kCVPixelFormatType_64ARGB: OSType { get }
var kCVPixelFormatType_48RGB: OSType { get }
var kCVPixelFormatType_32AlphaGray: OSType { get }
var kCVPixelFormatType_16Gray: OSType { get }
var kCVPixelFormatType_30RGB: OSType { get }
var kCVPixelFormatType_422YpCbCr8: OSType { get }
var kCVPixelFormatType_4444YpCbCrA8: OSType { get }
var kCVPixelFormatType_4444YpCbCrA8R: OSType { get }
var kCVPixelFormatType_4444AYpCbCr8: OSType { get }
var kCVPixelFormatType_4444AYpCbCr16: OSType { get }
var kCVPixelFormatType_444YpCbCr8: OSType { get }
var kCVPixelFormatType_422YpCbCr16: OSType { get }
var kCVPixelFormatType_422YpCbCr10: OSType { get }
var kCVPixelFormatType_444YpCbCr10: OSType { get }
var kCVPixelFormatType_420YpCbCr8Planar: OSType { get }
var kCVPixelFormatType_420YpCbCr8PlanarFullRange: OSType { get }
var kCVPixelFormatType_422YpCbCr_4A_8BiPlanar: OSType { get }
var kCVPixelFormatType_420YpCbCr8BiPlanarVideoRange: OSType { get }
var kCVPixelFormatType_420YpCbCr8BiPlanarFullRange: OSType { get }
var kCVPixelFormatType_422YpCbCr8_yuvs: OSType { get }
var kCVPixelFormatType_422YpCbCr8FullRange: OSType { get }
var kCVPixelFormatType_OneComponent8: OSType { get }
var kCVPixelFormatType_TwoComponent8: OSType { get }
var kCVPixelFormatType_OneComponent16Half: OSType { get }
var kCVPixelFormatType_OneComponent32Float: OSType { get }
var kCVPixelFormatType_TwoComponent16Half: OSType { get }
var kCVPixelFormatType_TwoComponent32Float: OSType { get }
var kCVPixelFormatType_64RGBAHalf: OSType { get }
var kCVPixelFormatType_128RGBAFloat: OSType { get }
typealias CVPixelBufferLockFlags = CVOptionFlags
var kCVPixelBufferLock_ReadOnly: CVPixelBufferLockFlags { get }
struct CVPlanarComponentInfo {
  var offset: Int32
  var rowBytes: UInt32
  init()
  init(offset offset: Int32, rowBytes rowBytes: UInt32)
}
struct CVPlanarPixelBufferInfo {
  var componentInfo: (CVPlanarComponentInfo)
  init()
  init(componentInfo componentInfo: (CVPlanarComponentInfo))
}
struct CVPlanarPixelBufferInfo_YCbCrPlanar {
  var componentInfoY: CVPlanarComponentInfo
  var componentInfoCb: CVPlanarComponentInfo
  var componentInfoCr: CVPlanarComponentInfo
  init()
  init(componentInfoY componentInfoY: CVPlanarComponentInfo, componentInfoCb componentInfoCb: CVPlanarComponentInfo, componentInfoCr componentInfoCr: CVPlanarComponentInfo)
}
struct CVPlanarPixelBufferInfo_YCbCrBiPlanar {
  var componentInfoY: CVPlanarComponentInfo
  var componentInfoCbCr: CVPlanarComponentInfo
  init()
  init(componentInfoY componentInfoY: CVPlanarComponentInfo, componentInfoCbCr componentInfoCbCr: CVPlanarComponentInfo)
}
@available(tvOS 4.0, *)
let kCVPixelBufferPixelFormatTypeKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferMemoryAllocatorKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferWidthKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferHeightKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferExtendedPixelsLeftKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferExtendedPixelsTopKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferExtendedPixelsRightKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferExtendedPixelsBottomKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferBytesPerRowAlignmentKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferCGBitmapContextCompatibilityKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferCGImageCompatibilityKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferOpenGLCompatibilityKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferPlaneAlignmentKey: CFString
@available(tvOS 4.0, *)
let kCVPixelBufferIOSurfacePropertiesKey: CFString
@available(tvOS 6.0, *)
let kCVPixelBufferOpenGLESCompatibilityKey: CFString
@available(tvOS 8.0, *)
let kCVPixelBufferMetalCompatibilityKey: CFString
@available(tvOS 9.0, *)
let kCVPixelBufferOpenGLESTextureCacheCompatibilityKey: CFString
typealias CVPixelBuffer = CVImageBuffer
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetTypeID() -> CFTypeID
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferCreateResolvedAttributesDictionary(_ allocator: CFAllocator?, _ attributes: CFArray?, _ resolvedDictionaryOut: UnsafeMutablePointer<CFDictionary?>) -> CVReturn
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferCreate(_ allocator: CFAllocator?, _ width: Int, _ height: Int, _ pixelFormatType: OSType, _ pixelBufferAttributes: CFDictionary?, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> CVReturn
typealias CVPixelBufferReleaseBytesCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<Void>?) -> Void
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferCreateWithBytes(_ allocator: CFAllocator?, _ width: Int, _ height: Int, _ pixelFormatType: OSType, _ baseAddress: UnsafeMutablePointer<Void>, _ bytesPerRow: Int, _ releaseCallback: CVPixelBufferReleaseBytesCallback?, _ releaseRefCon: UnsafeMutablePointer<Void>?, _ pixelBufferAttributes: CFDictionary?, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> CVReturn
typealias CVPixelBufferReleasePlanarBytesCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<Void>?, Int, Int, UnsafeMutablePointer<UnsafePointer<Void>?>!) -> Void
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferCreateWithPlanarBytes(_ allocator: CFAllocator?, _ width: Int, _ height: Int, _ pixelFormatType: OSType, _ dataPtr: UnsafeMutablePointer<Void>?, _ dataSize: Int, _ numberOfPlanes: Int, _ planeBaseAddress: UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!, _ planeWidth: UnsafeMutablePointer<Int>!, _ planeHeight: UnsafeMutablePointer<Int>!, _ planeBytesPerRow: UnsafeMutablePointer<Int>!, _ releaseCallback: CVPixelBufferReleasePlanarBytesCallback?, _ releaseRefCon: UnsafeMutablePointer<Void>?, _ pixelBufferAttributes: CFDictionary?, _ pixelBufferOut: UnsafeMutablePointer<CVPixelBuffer?>) -> CVReturn
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferLockBaseAddress(_ pixelBuffer: CVPixelBuffer, _ lockFlags: CVPixelBufferLockFlags) -> CVReturn
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferUnlockBaseAddress(_ pixelBuffer: CVPixelBuffer, _ unlockFlags: CVPixelBufferLockFlags) -> CVReturn
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetWidth(_ pixelBuffer: CVPixelBuffer) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetHeight(_ pixelBuffer: CVPixelBuffer) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetPixelFormatType(_ pixelBuffer: CVPixelBuffer) -> OSType
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetBaseAddress(_ pixelBuffer: CVPixelBuffer) -> UnsafeMutablePointer<Void>?
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetBytesPerRow(_ pixelBuffer: CVPixelBuffer) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetDataSize(_ pixelBuffer: CVPixelBuffer) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferIsPlanar(_ pixelBuffer: CVPixelBuffer) -> Bool
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetPlaneCount(_ pixelBuffer: CVPixelBuffer) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetWidthOfPlane(_ pixelBuffer: CVPixelBuffer, _ planeIndex: Int) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetHeightOfPlane(_ pixelBuffer: CVPixelBuffer, _ planeIndex: Int) -> Int
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetBaseAddressOfPlane(_ pixelBuffer: CVPixelBuffer, _ planeIndex: Int) -> UnsafeMutablePointer<Void>?
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferGetBytesPerRowOfPlane(_ pixelBuffer: CVPixelBuffer, _ planeIndex: Int) -> Int
@available(tvOS 4.0, *)
func CVPixelBufferGetExtendedPixels(_ pixelBuffer: CVPixelBuffer, _ extraColumnsOnLeft: UnsafeMutablePointer<Int>?, _ extraColumnsOnRight: UnsafeMutablePointer<Int>?, _ extraRowsOnTop: UnsafeMutablePointer<Int>?, _ extraRowsOnBottom: UnsafeMutablePointer<Int>?)
@available(tvOS 4.0, *)
@discardableResult
func CVPixelBufferFillExtendedPixels(_ pixelBuffer: CVPixelBuffer) -> CVReturn
