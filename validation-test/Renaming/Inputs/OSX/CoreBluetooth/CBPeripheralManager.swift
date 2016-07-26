
@available(OSX 10.9, *)
enum CBPeripheralAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case restricted
  case denied
  case authorized
}
@available(OSX 10.9, *)
enum CBPeripheralManagerState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case resetting
  case unsupported
  case unauthorized
  case poweredOff
  case poweredOn
}
@available(OSX 10.9, *)
enum CBPeripheralManagerConnectionLatency : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case low
  case medium
  case high
}
@available(OSX 10.9, *)
class CBPeripheralManager : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged CBPeripheralManagerDelegate?
  var state: CBPeripheralManagerState { get }
  var isAdvertising: Bool { get }
  init(delegate delegate: CBPeripheralManagerDelegate?, queue queue: dispatch_queue_t?)
  @available(OSX 10.9, *)
  init(delegate delegate: CBPeripheralManagerDelegate?, queue queue: dispatch_queue_t?, options options: [String : AnyObject]? = [:])
  func startAdvertising(_ advertisementData: [String : AnyObject]?)
  func stopAdvertising()
  func setDesiredConnectionLatency(_ latency: CBPeripheralManagerConnectionLatency, for central: CBCentral)
  func add(_ service: CBMutableService)
  func remove(_ service: CBMutableService)
  func removeAllServices()
  func respond(to request: CBATTRequest, withResult result: CBATTError)
  @discardableResult
  func updateValue(_ value: NSData, for characteristic: CBMutableCharacteristic, onSubscribedCentrals centrals: [CBCentral]?) -> Bool
}
protocol CBPeripheralManagerDelegate : NSObjectProtocol {
  @available(OSX 10.9, *)
  func peripheralManagerDidUpdateState(_ peripheral: CBPeripheralManager)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, willRestoreState dict: [String : AnyObject])
  @available(OSX 10.9, *)
  optional func peripheralManagerDidStartAdvertising(_ peripheral: CBPeripheralManager, error error: NSError?)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, didAdd service: CBService, error error: NSError?)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, central central: CBCentral, didSubscribeTo characteristic: CBCharacteristic)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, central central: CBCentral, didUnsubscribeFrom characteristic: CBCharacteristic)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, didReceiveRead request: CBATTRequest)
  @available(OSX 10.9, *)
  optional func peripheralManager(_ peripheral: CBPeripheralManager, didReceiveWrite requests: [CBATTRequest])
  @available(OSX 10.9, *)
  optional func peripheralManagerIsReady(toUpdateSubscribers peripheral: CBPeripheralManager)
}
