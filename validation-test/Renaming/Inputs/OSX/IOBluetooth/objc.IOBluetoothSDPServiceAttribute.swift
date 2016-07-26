
class IOBluetoothSDPServiceAttribute : NSObject, NSCoding {
  @discardableResult
  class func withID(_ newAttributeID: BluetoothSDPServiceAttributeID, attributeElementValue attributeElementValue: NSObject!) -> Self!
  @discardableResult
  class func withID(_ newAttributeID: BluetoothSDPServiceAttributeID, attributeElement attributeElement: IOBluetoothSDPDataElement!) -> Self!
  init!(id newAttributeID: BluetoothSDPServiceAttributeID, attributeElementValue attributeElementValue: NSObject!)
  init!(id newAttributeID: BluetoothSDPServiceAttributeID, attributeElement attributeElement: IOBluetoothSDPDataElement!)
  @discardableResult
  func getID() -> BluetoothSDPServiceAttributeID
  @discardableResult
  func getDataElement() -> IOBluetoothSDPDataElement!
  @discardableResult
  func getIDDataElement() -> IOBluetoothSDPDataElement!
}
