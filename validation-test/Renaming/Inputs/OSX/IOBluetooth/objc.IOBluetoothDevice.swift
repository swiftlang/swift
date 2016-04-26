
var kIOBluetoothDeviceNotificationNameConnected: String { get }
var kIOBluetoothDeviceNotificationNameDisconnected: String { get }
var kIOBluetoothDeviceNameChangedNotification: String { get }
var kIOBluetoothDeviceInquiryInfoChangedNotification: String { get }
var kIOBluetoothDeviceServicesChangedNotification: String { get }
var kIOBluetoothL2CAPChannelMaxAllowedIncomingMTU: String { get }
var kIOBluetoothL2CAPChannelDesiredOutgoingMTU: String { get }
protocol IOBluetoothDeviceAsyncCallbacks {
  func remoteNameRequestComplete(_ device: IOBluetoothDevice!, status status: IOReturn)
  func connectionComplete(_ device: IOBluetoothDevice!, status status: IOReturn)
  func sdpQueryComplete(_ device: IOBluetoothDevice!, status status: IOReturn)
}
class IOBluetoothDevice : IOBluetoothObject, NSCoding, NSSecureCoding {
  @discardableResult
  class func register(forConnectNotifications observer: AnyObject!, selector inSelector: Selector!) -> IOBluetoothUserNotification!
  @discardableResult
  func register(forDisconnectNotification observer: AnyObject!, selector inSelector: Selector!) -> IOBluetoothUserNotification!
  convenience init!(address address: UnsafePointer<BluetoothDeviceAddress>!)
  convenience init!(addressString address: String!)
  @discardableResult
  func openL2CAPChannelSync(_ newChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothL2CAPChannel?>!, withPSM psm: BluetoothL2CAPPSM, delegate channelDelegate: AnyObject!) -> IOReturn
  @discardableResult
  func openL2CAPChannelAsync(_ newChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothL2CAPChannel?>!, withPSM psm: BluetoothL2CAPPSM, delegate channelDelegate: AnyObject!) -> IOReturn
  @discardableResult
  func sendL2CAPEchoRequest(_ data: UnsafeMutablePointer<Void>!, length length: UInt16) -> IOReturn
  @discardableResult
  func openRFCOMMChannelSync(_ rfcommChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothRFCOMMChannel?>!, withChannelID channelID: BluetoothRFCOMMChannelID, delegate channelDelegate: AnyObject!) -> IOReturn
  @discardableResult
  func openRFCOMMChannelAsync(_ rfcommChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothRFCOMMChannel?>!, withChannelID channelID: BluetoothRFCOMMChannelID, delegate channelDelegate: AnyObject!) -> IOReturn
  var classOfDevice: BluetoothClassOfDevice { get }
  var serviceClassMajor: BluetoothServiceClassMajor { get }
  var deviceClassMajor: BluetoothDeviceClassMajor { get }
  var deviceClassMinor: BluetoothDeviceClassMinor { get }
  var name: String! { get }
  var nameOrAddress: String! { get }
  var lastNameUpdate: NSDate! { get }
  @discardableResult
  func getAddress() -> UnsafePointer<BluetoothDeviceAddress>!
  var addressString: String! { get }
  @discardableResult
  func getPageScanRepetitionMode() -> BluetoothPageScanRepetitionMode
  @discardableResult
  func getPageScanPeriodMode() -> BluetoothPageScanPeriodMode
  @discardableResult
  func getPageScanMode() -> BluetoothPageScanMode
  @discardableResult
  func getClockOffset() -> BluetoothClockOffset
  @discardableResult
  func getLastInquiryUpdate() -> NSDate!
  @available(OSX 10.7, *)
  @discardableResult
  func rssi() -> BluetoothHCIRSSIValue
  @available(OSX 10.7, *)
  @discardableResult
  func rawRSSI() -> BluetoothHCIRSSIValue
  @discardableResult
  func isConnected() -> Bool
  @discardableResult
  func openConnection() -> IOReturn
  @discardableResult
  func openConnection(_ target: AnyObject!) -> IOReturn
  @discardableResult
  func openConnection(_ target: AnyObject!, withPageTimeout pageTimeoutValue: BluetoothHCIPageTimeout, authenticationRequired authenticationRequired: Bool) -> IOReturn
  @discardableResult
  func closeConnection() -> IOReturn
  @discardableResult
  func remoteNameRequest(_ target: AnyObject!) -> IOReturn
  @discardableResult
  func remoteNameRequest(_ target: AnyObject!, withPageTimeout pageTimeoutValue: BluetoothHCIPageTimeout) -> IOReturn
  @discardableResult
  func requestAuthentication() -> IOReturn
  var connectionHandle: BluetoothConnectionHandle { get }
  @discardableResult
  func isIncoming() -> Bool
  @discardableResult
  func getLinkType() -> BluetoothLinkType
  @discardableResult
  func getEncryptionMode() -> BluetoothHCIEncryptionMode
  @discardableResult
  func performSDPQuery(_ target: AnyObject!) -> IOReturn
  @available(OSX 10.7, *)
  @discardableResult
  func performSDPQuery(_ target: AnyObject!, uuids uuidArray: [AnyObject]!) -> IOReturn
  var services: [AnyObject]! { get }
  @discardableResult
  func getLastServicesUpdate() -> NSDate!
  @discardableResult
  func getServiceRecord(for sdpUUID: IOBluetoothSDPUUID!) -> IOBluetoothSDPServiceRecord!
  @discardableResult
  class func favoriteDevices() -> [AnyObject]!
  @discardableResult
  func isFavorite() -> Bool
  @discardableResult
  func addToFavorites() -> IOReturn
  @discardableResult
  func removeFromFavorites() -> IOReturn
  @discardableResult
  class func recentDevices(_ numDevices: UInt) -> [AnyObject]!
  @discardableResult
  func recentAccessDate() -> NSDate!
  @discardableResult
  class func pairedDevices() -> [AnyObject]!
  @discardableResult
  func isPaired() -> Bool
  @discardableResult
  func setSupervisionTimeout(_ timeout: UInt16) -> IOReturn
  @discardableResult
  func openL2CAPChannelSync(_ newChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothL2CAPChannel?>!, withPSM psm: BluetoothL2CAPPSM, withConfiguration channelConfiguration: [NSObject : AnyObject]!, delegate channelDelegate: AnyObject!) -> IOReturn
  @discardableResult
  func openL2CAPChannelAsync(_ newChannel: AutoreleasingUnsafeMutablePointer<IOBluetoothL2CAPChannel?>!, withPSM psm: BluetoothL2CAPPSM, withConfiguration channelConfiguration: [NSObject : AnyObject]!, delegate channelDelegate: AnyObject!) -> IOReturn
}
