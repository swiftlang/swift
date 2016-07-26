
@discardableResult
func MPSSupportsMTLDevice(_ device: MTLDevice?) -> Bool
@available(iOS 9.0, *)
class MPSKernel : NSObject, NSCopying {
  var options: MPSKernelOptions
  var device: MTLDevice { get }
  var label: String?
  init(device device: MTLDevice)
  @discardableResult
  func copy(with zone: NSZone? = nil, device device: MTLDevice?) -> Self
}
