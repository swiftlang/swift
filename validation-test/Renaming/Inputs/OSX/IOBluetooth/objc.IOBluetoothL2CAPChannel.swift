
struct IOBluetoothL2CAPChannelEventType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kIOBluetoothL2CAPChannelEventTypeData: IOBluetoothL2CAPChannelEventType { get }
var kIOBluetoothL2CAPChannelEventTypeOpenComplete: IOBluetoothL2CAPChannelEventType { get }
var kIOBluetoothL2CAPChannelEventTypeClosed: IOBluetoothL2CAPChannelEventType { get }
var kIOBluetoothL2CAPChannelEventTypeReconfigured: IOBluetoothL2CAPChannelEventType { get }
var kIOBluetoothL2CAPChannelEventTypeWriteComplete: IOBluetoothL2CAPChannelEventType { get }
var kIOBluetoothL2CAPChannelEventTypeQueueSpaceAvailable: IOBluetoothL2CAPChannelEventType { get }
struct IOBluetoothL2CAPChannelDataBlock {
  var dataPtr: UnsafeMutablePointer<Void>!
  var dataSize: Int
  init()
  init(dataPtr dataPtr: UnsafeMutablePointer<Void>!, dataSize dataSize: Int)
}
struct IOBluetoothL2CAPChannelEvent {
  struct __Unnamed_union_u {
    var data: IOBluetoothL2CAPChannelDataBlock
    var writeRefCon: UnsafeMutablePointer<Void>!
    var padding: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
    init(data data: IOBluetoothL2CAPChannelDataBlock)
    init(writeRefCon writeRefCon: UnsafeMutablePointer<Void>!)
    init(padding padding: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
    init()
  }
  var eventType: IOBluetoothL2CAPChannelEventType
  var u: IOBluetoothL2CAPChannelEvent.__Unnamed_union_u
  var status: IOReturn
  init()
  init(eventType eventType: IOBluetoothL2CAPChannelEventType, u u: IOBluetoothL2CAPChannelEvent.__Unnamed_union_u, status status: IOReturn)
}
typealias IOBluetoothL2CAPChannelIncomingDataListener = @convention(c) (IOBluetoothL2CAPChannelRef!, UnsafeMutablePointer<Void>!, UInt16, UnsafeMutablePointer<Void>!) -> Void
typealias IOBluetoothL2CAPChannelIncomingEventListener = @convention(c) (IOBluetoothL2CAPChannelRef!, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<IOBluetoothL2CAPChannelEvent>!) -> Void
class IOBluetoothL2CAPChannel : IOBluetoothObject, NSPortDelegate {
  @discardableResult
  class func register(forChannelOpenNotifications object: AnyObject!, selector selector: Selector!) -> IOBluetoothUserNotification!
  @discardableResult
  class func register(forChannelOpenNotifications object: AnyObject!, selector selector: Selector!, withPSM psm: BluetoothL2CAPPSM, direction inDirection: IOBluetoothUserNotificationChannelDirection) -> IOBluetoothUserNotification!
  @discardableResult
  class func withObjectID(_ objectID: IOBluetoothObjectID) -> Self!
  @discardableResult
  func close() -> IOReturn
  var outgoingMTU: BluetoothL2CAPMTU { get }
  var incomingMTU: BluetoothL2CAPMTU { get }
  @discardableResult
  func requestRemoteMTU(_ remoteMTU: BluetoothL2CAPMTU) -> IOReturn
  @discardableResult
  func writeAsync(_ data: UnsafeMutablePointer<Void>!, length length: UInt16, refcon refcon: UnsafeMutablePointer<Void>!) -> IOReturn
  @discardableResult
  func writeSync(_ data: UnsafeMutablePointer<Void>!, length length: UInt16) -> IOReturn
  @discardableResult
  func setDelegate(_ channelDelegate: AnyObject!) -> IOReturn
  @discardableResult
  func setDelegate(_ channelDelegate: AnyObject!, withConfiguration channelConfiguration: [NSObject : AnyObject]!) -> IOReturn
  @discardableResult
  func delegate() -> AnyObject!
  var device: IOBluetoothDevice! { get }
  var objectID: IOBluetoothObjectID { get }
  var psm: BluetoothL2CAPPSM { get }
  var localChannelID: BluetoothL2CAPChannelID { get }
  var remoteChannelID: BluetoothL2CAPChannelID { get }
  @discardableResult
  func isIncoming() -> Bool
  @discardableResult
  func register(forChannelCloseNotification observer: AnyObject!, selector inSelector: Selector!) -> IOBluetoothUserNotification!
}
protocol IOBluetoothL2CAPChannelDelegate {
  optional func l2capChannelData(_ l2capChannel: IOBluetoothL2CAPChannel!, data dataPointer: UnsafeMutablePointer<Void>!, length dataLength: Int)
  optional func l2capChannelOpenComplete(_ l2capChannel: IOBluetoothL2CAPChannel!, status error: IOReturn)
  optional func l2capChannelClosed(_ l2capChannel: IOBluetoothL2CAPChannel!)
  optional func l2capChannelReconfigured(_ l2capChannel: IOBluetoothL2CAPChannel!)
  optional func l2capChannelWriteComplete(_ l2capChannel: IOBluetoothL2CAPChannel!, refcon refcon: UnsafeMutablePointer<Void>!, status error: IOReturn)
  optional func l2capChannelQueueSpaceAvailable(_ l2capChannel: IOBluetoothL2CAPChannel!)
}
let IOBluetoothL2CAPChannelPublishedNotification: String
let IOBluetoothL2CAPChannelTerminatedNotification: String
extension NSObject {
}
