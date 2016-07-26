
@available(OSX 10.7, *)
class CBService : NSObject {
  unowned(unsafe) var peripheral: @sil_unmanaged CBPeripheral { get }
  var uuid: CBUUID { get }
  var isPrimary: Bool { get }
  var includedServices: [CBService]? { get }
  var characteristics: [CBCharacteristic]? { get }
}
@available(OSX 10.9, *)
class CBMutableService : CBService {
  init(type UUID: CBUUID?, primary isPrimary: Bool)
}
