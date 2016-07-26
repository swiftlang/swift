
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
@available(iOS 5.0, *)
class CBCentralManager : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged CBCentralManagerDelegate?
  var state: CBCentralManagerState { get }
  @available(iOS 9.0, *)
  var isScanning: Bool { get }
  convenience init(delegate delegate: CBCentralManagerDelegate?, queue queue: dispatch_queue_t?)
  @available(iOS 7.0, *)
  init(delegate delegate: CBCentralManagerDelegate?, queue queue: dispatch_queue_t?, options options: [String : AnyObject]? = [:])
  @available(iOS 7.0, *)
  @discardableResult
  func retrievePeripherals(withIdentifiers identifiers: [NSUUID]) -> [CBPeripheral]
  @available(iOS 7.0, *)
  @discardableResult
  func retrieveConnectedPeripherals(withServices serviceUUIDs: [CBUUID]) -> [CBPeripheral]
  func scanForPeripherals(withServices serviceUUIDs: [CBUUID]?, options options: [String : AnyObject]? = [:])
  func stopScan()
  func connect(_ peripheral: CBPeripheral, options options: [String : AnyObject]? = [:])
  func cancelPeripheralConnection(_ peripheral: CBPeripheral)
}
protocol CBCentralManagerDelegate : NSObjectProtocol {
  @available(iOS 5.0, *)
  func centralManagerDidUpdateState(_ central: CBCentralManager)
  @available(iOS 5.0, *)
  optional func centralManager(_ central: CBCentralManager, willRestoreState dict: [String : AnyObject])
  @available(iOS 5.0, *)
  optional func centralManager(_ central: CBCentralManager, didDiscover peripheral: CBPeripheral, advertisementData advertisementData: [String : AnyObject], rssi RSSI: NSNumber)
  @available(iOS 5.0, *)
  optional func centralManager(_ central: CBCentralManager, didConnect peripheral: CBPeripheral)
  @available(iOS 5.0, *)
  optional func centralManager(_ central: CBCentralManager, didFailToConnect peripheral: CBPeripheral, error error: NSError?)
  @available(iOS 5.0, *)
  optional func centralManager(_ central: CBCentralManager, didDisconnectPeripheral peripheral: CBPeripheral, error error: NSError?)
}
