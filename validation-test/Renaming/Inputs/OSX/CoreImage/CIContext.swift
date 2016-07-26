
@available(OSX 10.4, *)
class CIContext : NSObject {
  @available(OSX 10.6, *)
  /*not inherited*/ init(cglContext cglctx: CGLContextObj, pixelFormat pixelFormat: CGLPixelFormatObj?, colorSpace colorSpace: CGColorSpace?, options options: [String : AnyObject]? = [:])
  @available(OSX 10.4, *)
  /*not inherited*/ init(cgContext cgctx: CGContext, options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  /*not inherited*/ init(options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  /*not inherited*/ init(mtlDevice device: MTLDevice)
  @available(OSX 10.11, *)
  /*not inherited*/ init(mtlDevice device: MTLDevice, options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  var workingColorSpace: CGColorSpace { get }
  func draw(_ image: CIImage, in inRect: CGRect, from fromRect: CGRect)
  @discardableResult
  func createCGImage(_ image: CIImage, from fromRect: CGRect) -> CGImage
  @discardableResult
  func createCGImage(_ image: CIImage, from fromRect: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace?) -> CGImage
  @available(OSX, introduced: 10.4, deprecated: 10.11)
  @discardableResult
  func createCGLayer(with size: CGSize, info info: CFDictionary?) -> CGLayer
  func render(_ image: CIImage, toBitmap data: UnsafeMutablePointer<Void>, rowBytes rowBytes: Int, bounds bounds: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace?)
  @available(OSX 10.6, *)
  func render(_ image: CIImage, to surface: IOSurface, bounds bounds: CGRect, colorSpace colorSpace: CGColorSpace?)
  @available(OSX 10.11, *)
  func render(_ image: CIImage, to buffer: CVPixelBuffer)
  @available(OSX 10.11, *)
  func render(_ image: CIImage, to buffer: CVPixelBuffer, bounds bounds: CGRect, colorSpace colorSpace: CGColorSpace?)
  @available(OSX 10.11, *)
  func render(_ image: CIImage, to texture: MTLTexture, commandBuffer commandBuffer: MTLCommandBuffer?, bounds bounds: CGRect, colorSpace colorSpace: CGColorSpace)
  @available(OSX 10.4, *)
  func reclaimResources()
  @available(OSX 10.4, *)
  func clearCaches()
}
let kCIContextOutputColorSpace: String
let kCIContextWorkingColorSpace: String
@available(OSX 10.4, *)
let kCIContextWorkingFormat: String
@available(OSX 10.11, *)
let kCIContextHighQualityDownsample: String
let kCIContextUseSoftwareRenderer: String
extension CIContext {
  @available(OSX 10.10, *)
  @discardableResult
  class func offlineGPUCount() -> UInt32
  @available(OSX 10.10, *)
  /*not inherited*/ init(forOfflineGPUAt index: UInt32)
  @available(OSX 10.10, *)
  /*not inherited*/ init(forOfflineGPUAt index: UInt32, colorSpace colorSpace: CGColorSpace?, options options: [String : AnyObject]? = [:], sharedContext sharedContext: CGLContextObj?)
}
