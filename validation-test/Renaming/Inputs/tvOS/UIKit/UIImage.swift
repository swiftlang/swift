
enum UIImageOrientation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case up
  case down
  case left
  case right
  case upMirrored
  case downMirrored
  case leftMirrored
  case rightMirrored
}
enum UIImageResizingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case tile
  case stretch
}
@available(tvOS 7.0, *)
enum UIImageRenderingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case alwaysOriginal
  case alwaysTemplate
}
@available(tvOS 2.0, *)
class UIImage : NSObject, NSSecureCoding {
  /*not inherited*/ init?(named name: String)
  @available(tvOS 8.0, *)
  /*not inherited*/ init?(named name: String, in bundle: NSBundle?, compatibleWith traitCollection: UITraitCollection?)
  init?(contentsOfFile path: String)
  init?(data data: NSData)
  @available(tvOS 6.0, *)
  init?(data data: NSData, scale scale: CGFloat)
  init(cgImage cgImage: CGImage)
  @available(tvOS 4.0, *)
  init(cgImage cgImage: CGImage, scale scale: CGFloat, orientation orientation: UIImageOrientation)
  @available(tvOS 5.0, *)
  init(ciImage ciImage: CIImage)
  @available(tvOS 6.0, *)
  init(ciImage ciImage: CIImage, scale scale: CGFloat, orientation orientation: UIImageOrientation)
  var size: CGSize { get }
  var cgImage: CGImage? { get }
  @available(tvOS 5.0, *)
  var ciImage: CIImage? { get }
  var imageOrientation: UIImageOrientation { get }
  @available(tvOS 4.0, *)
  var scale: CGFloat { get }
  @available(tvOS 5.0, *)
  @discardableResult
  class func animatedImageNamed(_ name: String, duration duration: NSTimeInterval) -> UIImage?
  @available(tvOS 5.0, *)
  @discardableResult
  class func animatedResizableImageNamed(_ name: String, capInsets capInsets: UIEdgeInsets, duration duration: NSTimeInterval) -> UIImage?
  @available(tvOS 6.0, *)
  @discardableResult
  class func animatedResizableImageNamed(_ name: String, capInsets capInsets: UIEdgeInsets, resizingMode resizingMode: UIImageResizingMode, duration duration: NSTimeInterval) -> UIImage?
  @available(tvOS 5.0, *)
  @discardableResult
  class func animatedImage(with images: [UIImage], duration duration: NSTimeInterval) -> UIImage?
  @available(tvOS 5.0, *)
  var images: [UIImage]? { get }
  @available(tvOS 5.0, *)
  var duration: NSTimeInterval { get }
  func draw(at point: CGPoint)
  func draw(at point: CGPoint, blendMode blendMode: CGBlendMode, alpha alpha: CGFloat)
  func draw(in rect: CGRect)
  func draw(in rect: CGRect, blendMode blendMode: CGBlendMode, alpha alpha: CGFloat)
  func drawAsPattern(in rect: CGRect)
  @available(tvOS 5.0, *)
  @discardableResult
  func resizableImage(withCapInsets capInsets: UIEdgeInsets) -> UIImage
  @available(tvOS 6.0, *)
  @discardableResult
  func resizableImage(withCapInsets capInsets: UIEdgeInsets, resizingMode resizingMode: UIImageResizingMode) -> UIImage
  @available(tvOS 5.0, *)
  var capInsets: UIEdgeInsets { get }
  @available(tvOS 6.0, *)
  var resizingMode: UIImageResizingMode { get }
  @available(tvOS 6.0, *)
  @discardableResult
  func withAlignmentRectInsets(_ alignmentInsets: UIEdgeInsets) -> UIImage
  @available(tvOS 6.0, *)
  var alignmentRectInsets: UIEdgeInsets { get }
  @available(tvOS 7.0, *)
  @discardableResult
  func withRenderingMode(_ renderingMode: UIImageRenderingMode) -> UIImage
  @available(tvOS 7.0, *)
  var renderingMode: UIImageRenderingMode { get }
  @available(tvOS 8.0, *)
  @NSCopying var traitCollection: UITraitCollection { get }
  @available(tvOS 8.0, *)
  var imageAsset: UIImageAsset? { get }
  @available(tvOS 9.0, *)
  @discardableResult
  func imageFlippedForRightToLeftLayoutDirection() -> UIImage
  @available(tvOS 9.0, *)
  var flipsForRightToLeftLayoutDirection: Bool { get }
}

extension UIImage : _ImageLiteralConvertible {
  convenience init!(failableImageLiteral name: String)
}
extension UIImage {
}
extension CIImage {
  @available(tvOS 5.0, *)
  init?(image image: UIImage)
  @available(tvOS 5.0, *)
  init?(image image: UIImage, options options: [NSObject : AnyObject]? = [:])
}
@discardableResult
func UIImagePNGRepresentation(_ image: UIImage) -> NSData?
@discardableResult
func UIImageJPEGRepresentation(_ image: UIImage, _ compressionQuality: CGFloat) -> NSData?
