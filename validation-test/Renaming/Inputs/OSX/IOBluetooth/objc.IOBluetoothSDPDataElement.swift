
class IOBluetoothSDPDataElement : NSObject, NSCoding {
  @discardableResult
  class func withElementValue(_ element: NSObject!) -> Self!
  @discardableResult
  class func withType(_ type: BluetoothSDPDataElementTypeDescriptor, sizeDescriptor newSizeDescriptor: BluetoothSDPDataElementSizeDescriptor, size newSize: UInt32, value newValue: NSObject!) -> Self!
  @discardableResult
  class func withSDPDataElementRef(_ sdpDataElementRef: IOBluetoothSDPDataElementRef!) -> Self!
  init!(elementValue element: NSObject!)
  init!(type newType: BluetoothSDPDataElementTypeDescriptor, sizeDescriptor newSizeDescriptor: BluetoothSDPDataElementSizeDescriptor, size newSize: UInt32, value newValue: NSObject!)
  @discardableResult
  func getRef() -> Unmanaged<IOBluetoothSDPDataElementRef>!
  @discardableResult
  func getTypeDescriptor() -> BluetoothSDPDataElementTypeDescriptor
  @discardableResult
  func getSizeDescriptor() -> BluetoothSDPDataElementSizeDescriptor
  @discardableResult
  func getSize() -> UInt32
  @discardableResult
  func getNumberValue() -> NSNumber!
  @discardableResult
  func getDataValue() -> NSData!
  @discardableResult
  func getStringValue() -> String!
  @discardableResult
  func getArrayValue() -> [AnyObject]!
  @discardableResult
  func getUUIDValue() -> IOBluetoothSDPUUID!
  @discardableResult
  func getValue() -> NSObject!
  @discardableResult
  func contains(_ dataElement: IOBluetoothSDPDataElement!) -> Bool
  @discardableResult
  func containsValue(_ cmpValue: NSObject!) -> Bool
}
