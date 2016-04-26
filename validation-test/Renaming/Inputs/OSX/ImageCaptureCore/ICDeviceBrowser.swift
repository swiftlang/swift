
protocol ICDeviceBrowserDelegate : NSObjectProtocol {
  func deviceBrowser(_ browser: ICDeviceBrowser, didAdd device: ICDevice, moreComing moreComing: Bool)
  func deviceBrowser(_ browser: ICDeviceBrowser, didRemove device: ICDevice, moreGoing moreGoing: Bool)
  optional func deviceBrowser(_ browser: ICDeviceBrowser, deviceDidChangeName device: ICDevice)
  optional func deviceBrowser(_ browser: ICDeviceBrowser, deviceDidChangeSharingState device: ICDevice)
  optional func deviceBrowser(_ browser: ICDeviceBrowser, requestsSelect device: ICDevice)
  optional func deviceBrowserDidEnumerateLocalDevices(_ browser: ICDeviceBrowser)
}
class ICDeviceBrowser : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged ICDeviceBrowserDelegate?
  var isBrowsing: Bool { get }
  var browsedDeviceTypeMask: ICDeviceTypeMask
  var devices: [ICDevice]? { get }
  @discardableResult
  func preferredDevice() -> ICDevice
  func start()
  func stop()
}
