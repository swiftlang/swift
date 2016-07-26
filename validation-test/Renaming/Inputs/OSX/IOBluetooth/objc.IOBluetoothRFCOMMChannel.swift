
class IOBluetoothRFCOMMChannel : IOBluetoothObject, NSPortDelegate {
  @discardableResult
  class func register(forChannelOpenNotifications object: AnyObject!, selector selector: Selector!) -> IOBluetoothUserNotification!
  @discardableResult
  class func register(forChannelOpenNotifications object: AnyObject!, selector selector: Selector!, withChannelID channelID: BluetoothRFCOMMChannelID, direction inDirection: IOBluetoothUserNotificationChannelDirection) -> IOBluetoothUserNotification!
  @discardableResult
  class func withRFCOMMChannelRef(_ rfcommChannelRef: IOBluetoothRFCOMMChannelRef!) -> Self!
  @discardableResult
  class func withObjectID(_ objectID: IOBluetoothObjectID) -> Self!
  @discardableResult
  func getRef() -> Unmanaged<IOBluetoothRFCOMMChannelRef>!
  @discardableResult
  func close() -> IOReturn
  @discardableResult
  func isOpen() -> Bool
  @discardableResult
  func getMTU() -> BluetoothRFCOMMMTU
  @discardableResult
  func isTransmissionPaused() -> Bool
  @discardableResult
  func writeAsync(_ data: UnsafeMutablePointer<Void>!, length length: UInt16, refcon refcon: UnsafeMutablePointer<Void>!) -> IOReturn
  @discardableResult
  func writeSync(_ data: UnsafeMutablePointer<Void>!, length length: UInt16) -> IOReturn
  @discardableResult
  func setSerialParameters(_ speed: UInt32, dataBits nBits: UInt8, parity parity: BluetoothRFCOMMParityType, stopBits bitStop: UInt8) -> IOReturn
  @discardableResult
  func sendRemoteLineStatus(_ lineStatus: BluetoothRFCOMMLineStatus) -> IOReturn
  @discardableResult
  func setDelegate(_ delegate: AnyObject!) -> IOReturn
  @discardableResult
  func delegate() -> AnyObject!
  @discardableResult
  func getID() -> BluetoothRFCOMMChannelID
  @discardableResult
  func isIncoming() -> Bool
  @discardableResult
  func getDevice() -> IOBluetoothDevice!
  @discardableResult
  func getObjectID() -> IOBluetoothObjectID
  @discardableResult
  func register(forChannelCloseNotification observer: AnyObject!, selector inSelector: Selector!) -> IOBluetoothUserNotification!
}
protocol IOBluetoothRFCOMMChannelDelegate {
  optional func rfcommChannelData(_ rfcommChannel: IOBluetoothRFCOMMChannel!, data dataPointer: UnsafeMutablePointer<Void>!, length dataLength: Int)
  optional func rfcommChannelOpenComplete(_ rfcommChannel: IOBluetoothRFCOMMChannel!, status error: IOReturn)
  optional func rfcommChannelClosed(_ rfcommChannel: IOBluetoothRFCOMMChannel!)
  optional func rfcommChannelControlSignalsChanged(_ rfcommChannel: IOBluetoothRFCOMMChannel!)
  optional func rfcommChannelFlowControlChanged(_ rfcommChannel: IOBluetoothRFCOMMChannel!)
  optional func rfcommChannelWriteComplete(_ rfcommChannel: IOBluetoothRFCOMMChannel!, refcon refcon: UnsafeMutablePointer<Void>!, status error: IOReturn)
  optional func rfcommChannelQueueSpaceAvailable(_ rfcommChannel: IOBluetoothRFCOMMChannel!)
}
