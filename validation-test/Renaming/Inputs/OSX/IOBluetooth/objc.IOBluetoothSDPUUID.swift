
class IOBluetoothSDPUUID : NSData {
  convenience init!(bytes bytes: UnsafePointer<Void>!, length length: UInt32)
  convenience init!(data data: NSData!)
  @discardableResult
  class func uuid16(_ uuid16: BluetoothSDPUUID16) -> Self!
  @discardableResult
  class func uuid32(_ uuid32: BluetoothSDPUUID32) -> Self!
  init!(uuid16 uuid16: BluetoothSDPUUID16)
  init!(uuid32 uuid32: BluetoothSDPUUID32)
  @discardableResult
  func getWithLength(_ newLength: UInt32) -> Self!
  @discardableResult
  func isEqual(to otherUUID: IOBluetoothSDPUUID!) -> Bool
}
