
@available(OSX 10.4, *)
class CIImage : NSObject, NSSecureCoding, NSCopying {
  @discardableResult
  class func empty() -> CIImage
  init(cgImage image: CGImage)
  init(cgImage image: CGImage, options options: [String : AnyObject]? = [:])
  @available(OSX, introduced: 10.4, deprecated: 10.11, message: "Use initWithCGImage: instead.")
  init(cgLayer layer: CGLayer)
  @available(OSX, introduced: 10.4, deprecated: 10.11, message: "Use initWithCGImage:options instead.")
  init(cgLayer layer: CGLayer, options options: [String : AnyObject]? = [:])
  init?(data data: NSData)
  init?(data data: NSData, options options: [String : AnyObject]? = [:])
  init(bitmapData data: NSData, bytesPerRow bytesPerRow: Int, size size: CGSize, format format: CIFormat, colorSpace colorSpace: CGColorSpace?)
  @available(OSX 10.4, *)
  init(texture name: UInt32, size size: CGSize, flipped flipped: Bool, colorSpace colorSpace: CGColorSpace?)
  @available(OSX 10.9, *)
  init(texture name: UInt32, size size: CGSize, flipped flipped: Bool, options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  init(mtlTexture texture: MTLTexture, options options: [String : AnyObject]? = [:])
  init?(contentsOf url: NSURL)
  init?(contentsOf url: NSURL, options options: [String : AnyObject]? = [:])
  @available(OSX 10.6, *)
  init(ioSurface surface: IOSurface)
  @available(OSX 10.6, *)
  init(ioSurface surface: IOSurface, options options: [String : AnyObject]? = [:])
  @available(OSX, introduced: 10.9, deprecated: 10.11)
  init(ioSurface surface: IOSurface, plane plane: Int, format format: CIFormat, options options: [String : AnyObject]? = [:])
  @available(OSX 10.4, *)
  init(cvImageBuffer imageBuffer: CVImageBuffer)
  @available(OSX 10.4, *)
  init(cvImageBuffer imageBuffer: CVImageBuffer, options options: [String : AnyObject]? = [:])
  @available(OSX 10.11, *)
  init(cvPixelBuffer pixelBuffer: CVPixelBuffer)
  @available(OSX 10.11, *)
  init(cvPixelBuffer pixelBuffer: CVPixelBuffer, options options: [String : AnyObject]? = [:])
  init(color color: CIColor)
  @discardableResult
  func applying(_ matrix: CGAffineTransform) -> CIImage
  @available(OSX 10.10, *)
  @discardableResult
  func applyingOrientation(_ orientation: Int32) -> CIImage
  @available(OSX 10.10, *)
  @discardableResult
  func imageTransform(forOrientation orientation: Int32) -> CGAffineTransform
  @available(OSX 10.4, *)
  @discardableResult
  func compositingOverImage(_ dest: CIImage) -> CIImage
  @discardableResult
  func cropping(to rect: CGRect) -> CIImage
  @available(OSX 10.10, *)
  @discardableResult
  func clampingToExtent() -> CIImage
  @available(OSX 10.10, *)
  @discardableResult
  func applyingFilter(_ filterName: String, withInputParameters params: [String : AnyObject]?) -> CIImage
  var extent: CGRect { get }
  @available(OSX 10.8, *)
  var properties: [String : AnyObject] { get }
  @available(OSX 10.4, *)
  var definition: CIFilterShape { get }
  @available(OSX 10.4, *)
  var url: NSURL? { get }
  @available(OSX 10.4, *)
  var colorSpace: CGColorSpace? { get }
  @available(OSX 10.11, *)
  @discardableResult
  func regionOfInterest(for image: CIImage, in rect: CGRect) -> CGRect
}
typealias CIFormat = Int32
@available(OSX 10.4, *)
var kCIFormatARGB8: CIFormat
var kCIFormatBGRA8: CIFormat
var kCIFormatRGBA8: CIFormat
@available(OSX 10.11, *)
var kCIFormatABGR8: CIFormat
@available(OSX 10.4, *)
var kCIFormatRGBA16: CIFormat
@available(OSX 10.4, *)
var kCIFormatRGBAf: CIFormat
@available(OSX 10.4, *)
var kCIFormatRGBAh: CIFormat
@available(OSX 10.11, *)
var kCIFormatA8: CIFormat
@available(OSX 10.11, *)
var kCIFormatA16: CIFormat
@available(OSX 10.11, *)
var kCIFormatAh: CIFormat
@available(OSX 10.11, *)
var kCIFormatAf: CIFormat
@available(OSX 10.11, *)
var kCIFormatR8: CIFormat
@available(OSX 10.11, *)
var kCIFormatR16: CIFormat
@available(OSX 10.11, *)
var kCIFormatRh: CIFormat
@available(OSX 10.11, *)
var kCIFormatRf: CIFormat
@available(OSX 10.11, *)
var kCIFormatRG8: CIFormat
@available(OSX 10.11, *)
var kCIFormatRG16: CIFormat
@available(OSX 10.11, *)
var kCIFormatRGh: CIFormat
@available(OSX 10.11, *)
var kCIFormatRGf: CIFormat
let kCIImageColorSpace: String
@available(OSX 10.8, *)
let kCIImageProperties: String
@available(OSX 10.9, *)
let kCIImageTextureTarget: String
@available(OSX 10.9, *)
let kCIImageTextureFormat: String
extension CIImage {
  @available(OSX 10.8, *)
  @discardableResult
  func autoAdjustmentFilters(options options: [String : AnyObject]? = [:]) -> [CIFilter]
}
@available(OSX 10.8, *)
let kCIImageAutoAdjustEnhance: String
@available(OSX 10.8, *)
let kCIImageAutoAdjustRedEye: String
@available(OSX 10.8, *)
let kCIImageAutoAdjustFeatures: String
@available(OSX 10.10, *)
let kCIImageAutoAdjustCrop: String
@available(OSX 10.10, *)
let kCIImageAutoAdjustLevel: String
