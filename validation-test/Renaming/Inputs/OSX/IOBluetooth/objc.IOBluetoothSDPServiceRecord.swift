
class IOBluetoothSDPServiceRecord : NSObject, NSCoding {
  @discardableResult
  class func publishedServiceRecord(with serviceDict: [NSObject : AnyObject]!) -> Self!
  @discardableResult
  func remove() -> IOReturn
  @discardableResult
  class func withServiceDictionary(_ serviceDict: [NSObject : AnyObject]!, device device: IOBluetoothDevice!) -> Self!
  init!(serviceDictionary serviceDict: [NSObject : AnyObject]!, device device: IOBluetoothDevice!)
  @discardableResult
  class func withSDPServiceRecordRef(_ sdpServiceRecordRef: IOBluetoothSDPServiceRecordRef!) -> Self!
  @discardableResult
  func getRef() -> Unmanaged<IOBluetoothSDPServiceRecordRef>!
  var device: IOBluetoothDevice! { get }
  var attributes: [NSObject : AnyObject]! { get }
  @discardableResult
  func getAttributeDataElement(_ attributeID: BluetoothSDPServiceAttributeID) -> IOBluetoothSDPDataElement!
  @discardableResult
  func getServiceName() -> String!
  @discardableResult
  func getRFCOMMChannelID(_ rfcommChannelID: UnsafeMutablePointer<BluetoothRFCOMMChannelID>!) -> IOReturn
  @discardableResult
  func getL2CAPPSM(_ outPSM: UnsafeMutablePointer<BluetoothL2CAPPSM>!) -> IOReturn
  @discardableResult
  func getHandle(_ outServiceRecordHandle: UnsafeMutablePointer<BluetoothSDPServiceRecordHandle>!) -> IOReturn
  @discardableResult
  func matchesUUID16(_ uuid16: BluetoothSDPUUID16) -> Bool
  @discardableResult
  func matchesUUIDArray(_ uuidArray: [AnyObject]!) -> Bool
  @discardableResult
  func matchesSearch(_ searchArray: [AnyObject]!) -> Bool
  @discardableResult
  func hasService(from array: [AnyObject]!) -> Bool
  var sortedAttributes: [AnyObject]! { get }
}
