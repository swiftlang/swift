
@available(iOS 5.0, *)
class CIImage : NSObject, NSSecureCoding, NSCopying {
  @discardableResult
  class func empty() -> CIImage
  init(cgImage image: CGImage)
  init(cgImage image: CGImage, options options: [String : AnyObject]? = [:])
  init?(data data: NSData)
  init?(data data: NSData, options options: [String : AnyObject]? = [:])
  init(bitmapData data: NSData, bytesPerRow bytesPerRow: Int, size size: CGSize, format format: CIFormat, colorSpace colorSpace: CGColorSpace?)
  @available(iOS 6.0, *)
  init(texture name: UInt32, size size: CGSize, flipped flipped: Bool, colorSpace colorSpace: CGColorSpace?)
  @available(iOS 9.0, *)
  init(mtlTexture texture: MTLTexture, options options: [String : AnyObject]? = [:])
  init?(contentsOf url: NSURL)
  init?(contentsOf url: NSURL, options options: [String : AnyObject]? = [:])
  @available(iOS 9.0, *)
  init(cvImageBuffer imageBuffer: CVImageBuffer)
  @available(iOS 9.0, *)
  init(cvImageBuffer imageBuffer: CVImageBuffer, options options: [String : AnyObject]? = [:])
  @available(iOS 5.0, *)
  init(cvPixelBuffer pixelBuffer: CVPixelBuffer)
  @available(iOS 5.0, *)
  init(cvPixelBuffer pixelBuffer: CVPixelBuffer, options options: [String : AnyObject]? = [:])
  init(color color: CIColor)
  @discardableResult
  func applying(_ matrix: CGAffineTransform) -> CIImage
  @available(iOS 8.0, *)
  @discardableResult
  func applyingOrientation(_ orientation: Int32) -> CIImage
  @available(iOS 8.0, *)
  @discardableResult
  func imageTransform(forOrientation orientation: Int32) -> CGAffineTransform
  @available(iOS 8.0, *)
  @discardableResult
  func compositingOverImage(_ dest: CIImage) -> CIImage
  @discardableResult
  func cropping(to rect: CGRect) -> CIImage
  @available(iOS 8.0, *)
  @discardableResult
  func clampingToExtent() -> CIImage
  @available(iOS 8.0, *)
  @discardableResult
  func applyingFilter(_ filterName: String, withInputParameters params: [String : AnyObject]?) -> CIImage
  var extent: CGRect { get }
  @available(iOS 5.0, *)
  var properties: [String : AnyObject] { get }
  @available(iOS 9.0, *)
  var url: NSURL? { get }
  @available(iOS 9.0, *)
  var colorSpace: CGColorSpace? { get }
  @available(iOS 6.0, *)
  @discardableResult
  func regionOfInterest(for image: CIImage, in rect: CGRect) -> CGRect
}
typealias CIFormat = Int32
@available(iOS 6.0, *)
var kCIFormatARGB8: CIFormat
var kCIFormatBGRA8: CIFormat
var kCIFormatRGBA8: CIFormat
@available(iOS 9.0, *)
var kCIFormatABGR8: CIFormat
@available(iOS 7.0, *)
var kCIFormatRGBAf: CIFormat
@available(iOS 6.0, *)
var kCIFormatRGBAh: CIFormat
@available(iOS 9.0, *)
var kCIFormatA8: CIFormat
@available(iOS 9.0, *)
var kCIFormatA16: CIFormat
@available(iOS 9.0, *)
var kCIFormatAh: CIFormat
@available(iOS 9.0, *)
var kCIFormatAf: CIFormat
@available(iOS 9.0, *)
var kCIFormatR8: CIFormat
@available(iOS 9.0, *)
var kCIFormatR16: CIFormat
@available(iOS 9.0, *)
var kCIFormatRh: CIFormat
@available(iOS 9.0, *)
var kCIFormatRf: CIFormat
@available(iOS 9.0, *)
var kCIFormatRG8: CIFormat
@available(iOS 9.0, *)
var kCIFormatRG16: CIFormat
@available(iOS 9.0, *)
var kCIFormatRGh: CIFormat
@available(iOS 9.0, *)
var kCIFormatRGf: CIFormat
let kCIImageColorSpace: String
@available(iOS 5.0, *)
let kCIImageProperties: String
extension CIImage {
  @available(iOS 5.0, *)
  @discardableResult
  func autoAdjustmentFilters(options options: [String : AnyObject]? = [:]) -> [CIFilter]
}
@available(iOS 5.0, *)
let kCIImageAutoAdjustEnhance: String
@available(iOS 5.0, *)
let kCIImageAutoAdjustRedEye: String
@available(iOS 5.0, *)
let kCIImageAutoAdjustFeatures: String
@available(iOS 8.0, *)
let kCIImageAutoAdjustCrop: String
@available(iOS 8.0, *)
let kCIImageAutoAdjustLevel: String
