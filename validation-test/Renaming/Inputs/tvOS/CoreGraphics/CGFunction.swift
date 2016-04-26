
class CGFunction {
}
typealias CGFunctionEvaluateCallback = @convention(c) (UnsafeMutablePointer<Void>?, UnsafePointer<CGFloat>, UnsafeMutablePointer<CGFloat>) -> Void
typealias CGFunctionReleaseInfoCallback = @convention(c) (UnsafeMutablePointer<Void>?) -> Void
struct CGFunctionCallbacks {
  var version: UInt32
  var evaluate: CGFunctionEvaluateCallback?
  var releaseInfo: CGFunctionReleaseInfoCallback?
  init()
  init(version version: UInt32, evaluate evaluate: CGFunctionEvaluateCallback?, releaseInfo releaseInfo: CGFunctionReleaseInfoCallback?)
}
extension CGFunction {
  @available(tvOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(tvOS 2.0, *)
  init?(info info: UnsafeMutablePointer<Void>?, domainDimension domainDimension: Int, domain domain: UnsafePointer<CGFloat>?, rangeDimension rangeDimension: Int, range range: UnsafePointer<CGFloat>?, callbacks callbacks: UnsafePointer<CGFunctionCallbacks>?)
}
