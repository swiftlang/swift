
class IOBluetoothHostController : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged AnyObject!
  @discardableResult
  class func defaultController() -> Self!
  var powerState: BluetoothHCIPowerState { get }
  @discardableResult
  func classOfDevice() -> BluetoothClassOfDevice
  @discardableResult
  func setClassOfDevice(_ classOfDevice: BluetoothClassOfDevice, forTimeInterval seconds: NSTimeInterval) -> IOReturn
  @discardableResult
  func addressAsString() -> String!
  @discardableResult
  func nameAsString() -> String!
}
extension NSObject {
  class func readRSSI(forDeviceComplete controller: AnyObject!, device device: IOBluetoothDevice!, info info: UnsafeMutablePointer<BluetoothHCIRSSIInfo>!, error error: IOReturn)
  func readRSSI(forDeviceComplete controller: AnyObject!, device device: IOBluetoothDevice!, info info: UnsafeMutablePointer<BluetoothHCIRSSIInfo>!, error error: IOReturn)
  class func readLinkQuality(forDeviceComplete controller: AnyObject!, device device: IOBluetoothDevice!, info info: UnsafeMutablePointer<BluetoothHCILinkQualityInfo>!, error error: IOReturn)
  func readLinkQuality(forDeviceComplete controller: AnyObject!, device device: IOBluetoothDevice!, info info: UnsafeMutablePointer<BluetoothHCILinkQualityInfo>!, error error: IOReturn)
}
let IOBluetoothHostControllerPoweredOnNotification: String
let IOBluetoothHostControllerPoweredOffNotification: String
