
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
@available(watchOS 2.0, *)
enum UIImageRenderingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case automatic
  case alwaysOriginal
  case alwaysTemplate
}
@available(watchOS 2.0, *)
class UIImage : NSObject, NSSecureCoding {
  /*not inherited*/ init?(named name: String)
  init?(contentsOfFile path: String)
  init?(data data: NSData)
  @available(watchOS 2.0, *)
  init?(data data: NSData, scale scale: CGFloat)
  init(cgImage cgImage: CGImage)
  @available(watchOS 2.0, *)
  init(cgImage cgImage: CGImage, scale scale: CGFloat, orientation orientation: UIImageOrientation)
  var size: CGSize { get }
  var cgImage: CGImage? { get }
  var imageOrientation: UIImageOrientation { get }
  @available(watchOS 2.0, *)
  var scale: CGFloat { get }
  @available(watchOS 2.0, *)
  @discardableResult
  class func animatedImageNamed(_ name: String, duration duration: NSTimeInterval) -> UIImage?
  @available(watchOS 2.0, *)
  @discardableResult
  class func animatedResizableImageNamed(_ name: String, capInsets capInsets: UIEdgeInsets, duration duration: NSTimeInterval) -> UIImage?
  @available(watchOS 2.0, *)
  @discardableResult
  class func animatedResizableImageNamed(_ name: String, capInsets capInsets: UIEdgeInsets, resizingMode resizingMode: UIImageResizingMode, duration duration: NSTimeInterval) -> UIImage?
  @available(watchOS 2.0, *)
  @discardableResult
  class func animatedImage(with images: [UIImage], duration duration: NSTimeInterval) -> UIImage?
  @available(watchOS 2.0, *)
  var images: [UIImage]? { get }
  @available(watchOS 2.0, *)
  var duration: NSTimeInterval { get }
  func draw(at point: CGPoint)
  func draw(at point: CGPoint, blendMode blendMode: CGBlendMode, alpha alpha: CGFloat)
  func draw(in rect: CGRect)
  func draw(in rect: CGRect, blendMode blendMode: CGBlendMode, alpha alpha: CGFloat)
  func drawAsPattern(in rect: CGRect)
  @available(watchOS 2.0, *)
  @discardableResult
  func resizableImage(withCapInsets capInsets: UIEdgeInsets) -> UIImage
  @available(watchOS 2.0, *)
  @discardableResult
  func resizableImage(withCapInsets capInsets: UIEdgeInsets, resizingMode resizingMode: UIImageResizingMode) -> UIImage
  @available(watchOS 2.0, *)
  var capInsets: UIEdgeInsets { get }
  @available(watchOS 2.0, *)
  var resizingMode: UIImageResizingMode { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func withAlignmentRectInsets(_ alignmentInsets: UIEdgeInsets) -> UIImage
  @available(watchOS 2.0, *)
  var alignmentRectInsets: UIEdgeInsets { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func withRenderingMode(_ renderingMode: UIImageRenderingMode) -> UIImage
  @available(watchOS 2.0, *)
  var renderingMode: UIImageRenderingMode { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func imageFlippedForRightToLeftLayoutDirection() -> UIImage
  @available(watchOS 2.0, *)
  var flipsForRightToLeftLayoutDirection: Bool { get }
}

extension UIImage : _ImageLiteralConvertible {
  convenience init!(failableImageLiteral name: String)
}
extension UIImage {
  @discardableResult
  func stretchableImage(withLeftCapWidth leftCapWidth: Int, topCapHeight topCapHeight: Int) -> UIImage
  var leftCapWidth: Int { get }
  var topCapHeight: Int { get }
}
@discardableResult
func UIImagePNGRepresentation(_ image: UIImage) -> NSData?
@discardableResult
func UIImageJPEGRepresentation(_ image: UIImage, _ compressionQuality: CGFloat) -> NSData?
