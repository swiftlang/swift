
@available(tvOS 5.0, *)
class CIContext : NSObject {
  @available(tvOS 9.0, *)
  /*not inherited*/ init(cgContext cgctx: CGContext, options options: [String : AnyObject]? = [:])
  @available(tvOS 5.0, *)
  /*not inherited*/ init(options options: [String : AnyObject]? = [:])
  @available(tvOS 5.0, *)
  /*not inherited*/ init(eaglContext eaglContext: EAGLContext)
  @available(tvOS 5.0, *)
  /*not inherited*/ init(eaglContext eaglContext: EAGLContext, options options: [String : AnyObject]? = [:])
  @available(tvOS 9.0, *)
  /*not inherited*/ init(mtlDevice device: MTLDevice)
  @available(tvOS 9.0, *)
  /*not inherited*/ init(mtlDevice device: MTLDevice, options options: [String : AnyObject]? = [:])
  @available(tvOS 9.0, *)
  var workingColorSpace: CGColorSpace { get }
  func draw(_ image: CIImage, in inRect: CGRect, from fromRect: CGRect)
  @discardableResult
  func createCGImage(_ image: CIImage, from fromRect: CGRect) -> CGImage
  @discardableResult
  func createCGImage(_ image: CIImage, from fromRect: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace?) -> CGImage
  func render(_ image: CIImage, toBitmap data: UnsafeMutablePointer<Void>, rowBytes rowBytes: Int, bounds bounds: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace?)
  @available(tvOS 5.0, *)
  func render(_ image: CIImage, to buffer: CVPixelBuffer)
  @available(tvOS 5.0, *)
  func render(_ image: CIImage, to buffer: CVPixelBuffer, bounds bounds: CGRect, colorSpace colorSpace: CGColorSpace?)
  @available(tvOS 9.0, *)
  func render(_ image: CIImage, to texture: MTLTexture, commandBuffer commandBuffer: MTLCommandBuffer?, bounds bounds: CGRect, colorSpace colorSpace: CGColorSpace)
  @available(tvOS 5.0, *)
  @discardableResult
  func inputImageMaximumSize() -> CGSize
  @available(tvOS 5.0, *)
  @discardableResult
  func outputImageMaximumSize() -> CGSize
}
let kCIContextOutputColorSpace: String
let kCIContextWorkingColorSpace: String
@available(tvOS 8.0, *)
let kCIContextWorkingFormat: String
@available(tvOS 9.0, *)
let kCIContextHighQualityDownsample: String
let kCIContextUseSoftwareRenderer: String
@available(tvOS 8.0, *)
let kCIContextPriorityRequestLow: String
extension CIContext {
}
