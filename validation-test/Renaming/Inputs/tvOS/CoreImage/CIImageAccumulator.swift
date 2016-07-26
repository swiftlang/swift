
@available(tvOS 9.0, *)
class CIImageAccumulator : NSObject {
  init(extent extent: CGRect, format format: CIFormat)
  @available(tvOS 9.0, *)
  init(extent extent: CGRect, format format: CIFormat, colorSpace colorSpace: CGColorSpace)
  var extent: CGRect { get }
  var format: CIFormat { get }
  @discardableResult
  func image() -> CIImage
  func setImage(_ image: CIImage)
  func setImage(_ image: CIImage, dirtyRect dirtyRect: CGRect)
  func clear()
}
