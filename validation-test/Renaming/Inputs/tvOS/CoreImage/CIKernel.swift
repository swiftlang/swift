
typealias CIKernelROICallback = (Int32, CGRect) -> CGRect
@available(tvOS 8.0, *)
class CIKernel : NSObject {
  @available(tvOS 8.0, *)
  @discardableResult
  class func kernels(with string: String) -> [CIKernel]?
  @available(tvOS 8.0, *)
  convenience init?(string string: String)
  @available(tvOS 8.0, *)
  var name: String { get }
  @available(tvOS 9.0, *)
  func setROISelector(_ method: Selector)
  @available(tvOS 8.0, *)
  @discardableResult
  func apply(withExtent extent: CGRect, roiCallback callback: CIKernelROICallback, arguments args: [AnyObject]?) -> CIImage?
}
@available(tvOS 8.0, *)
class CIColorKernel : CIKernel {
  @available(tvOS 8.0, *)
  @discardableResult
  func apply(withExtent extent: CGRect, arguments args: [AnyObject]?) -> CIImage?
}
@available(tvOS 8.0, *)
class CIWarpKernel : CIKernel {
  @available(tvOS 8.0, *)
  @discardableResult
  func apply(withExtent extent: CGRect, roiCallback callback: CIKernelROICallback, inputImage image: CIImage, arguments args: [AnyObject]?) -> CIImage?
}
