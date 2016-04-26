
@available(tvOS 5.0, *)
class CBService : CBAttribute {
  unowned(unsafe) var peripheral: @sil_unmanaged CBPeripheral { get }
  var isPrimary: Bool { get }
  var includedServices: [CBService]? { get }
  var characteristics: [CBCharacteristic]? { get }
}
@available(tvOS 6.0, *)
class CBMutableService : CBService {
}
