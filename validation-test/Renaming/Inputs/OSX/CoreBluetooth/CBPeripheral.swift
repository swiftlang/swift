
@available(OSX 10.9, *)
enum CBPeripheralState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case disconnected
  case connecting
  case connected
}
enum CBCharacteristicWriteType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case withResponse
  case withoutResponse
}
@available(OSX 10.7, *)
class CBPeripheral : NSObject, NSCopying {
  unowned(unsafe) var delegate: @sil_unmanaged CBPeripheralDelegate?
  var identifier: NSUUID { get }
  var name: String? { get }
  var rssi: NSNumber? { get }
  var state: CBPeripheralState { get }
  var services: [CBService]? { get }
  func readRSSI()
  func discoverServices(_ serviceUUIDs: [CBUUID]?)
  func discoverIncludedServices(_ includedServiceUUIDs: [CBUUID]?, for service: CBService)
  func discoverCharacteristics(_ characteristicUUIDs: [CBUUID]?, for service: CBService)
  func readValue(for characteristic: CBCharacteristic)
  func writeValue(_ data: NSData, for characteristic: CBCharacteristic, type type: CBCharacteristicWriteType)
  func setNotifyValue(_ enabled: Bool, for characteristic: CBCharacteristic)
  func discoverDescriptors(for characteristic: CBCharacteristic)
  func readValue(for descriptor: CBDescriptor)
  func writeValue(_ data: NSData, for descriptor: CBDescriptor)
}
protocol CBPeripheralDelegate : NSObjectProtocol {
  @available(OSX 10.9, *)
  optional func peripheralDidUpdateName(_ peripheral: CBPeripheral)
  @available(OSX 10.9, *)
  optional func peripheral(_ peripheral: CBPeripheral, didModifyServices invalidatedServices: [CBService])
  @available(OSX 10.7, *)
  optional func peripheralDidUpdateRSSI(_ peripheral: CBPeripheral, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didDiscoverServices error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didDiscoverIncludedServicesFor service: CBService, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didDiscoverCharacteristicsFor service: CBService, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didUpdateValueFor characteristic: CBCharacteristic, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didWriteValueFor characteristic: CBCharacteristic, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didUpdateNotificationStateFor characteristic: CBCharacteristic, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didDiscoverDescriptorsFor characteristic: CBCharacteristic, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didUpdateValueFor descriptor: CBDescriptor, error error: NSError?)
  @available(OSX 10.7, *)
  optional func peripheral(_ peripheral: CBPeripheral, didWriteValueFor descriptor: CBDescriptor, error error: NSError?)
}
