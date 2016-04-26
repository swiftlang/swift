
enum CBCentralManagerState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case resetting
  case unsupported
  case unauthorized
  case poweredOff
  case poweredOn
}
@available(OSX 10.7, *)
class CBCentralManager : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged CBCentralManagerDelegate?
  var state: CBCentralManagerState { get }
  init(delegate delegate: CBCentralManagerDelegate?, queue queue: dispatch_queue_t?)
  @available(OSX 10.9, *)
  init(delegate delegate: CBCentralManagerDelegate?, queue queue: dispatch_queue_t?, options options: [String : AnyObject]? = [:])
  @available(OSX 10.9, *)
  @discardableResult
  func retrievePeripherals(withIdentifiers identifiers: [NSUUID]) -> [CBPeripheral]
  @available(OSX 10.9, *)
  @discardableResult
  func retrieveConnectedPeripherals(withServices serviceUUIDs: [CBUUID]) -> [CBPeripheral]
  func scanForPeripherals(withServices serviceUUIDs: [CBUUID]?, options options: [String : AnyObject]? = [:])
  func stopScan()
  func connect(_ peripheral: CBPeripheral, options options: [String : AnyObject]? = [:])
  func cancelPeripheralConnection(_ peripheral: CBPeripheral)
}
protocol CBCentralManagerDelegate : NSObjectProtocol {
  @available(OSX 10.7, *)
  func centralManagerDidUpdateState(_ central: CBCentralManager)
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, willRestoreState dict: [String : AnyObject])
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didRetrievePeripherals peripherals: [CBPeripheral])
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didRetrieveConnectedPeripherals peripherals: [CBPeripheral])
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didDiscover peripheral: CBPeripheral, advertisementData advertisementData: [String : AnyObject], rssi RSSI: NSNumber)
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didConnect peripheral: CBPeripheral)
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didFailToConnect peripheral: CBPeripheral, error error: NSError?)
  @available(OSX 10.7, *)
  optional func centralManager(_ central: CBCentralManager, didDisconnectPeripheral peripheral: CBPeripheral, error error: NSError?)
}
