
enum ICDeviceType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case camera
  case scanner
}
enum ICDeviceLocationType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case local
  case shared
  case bonjour
  case bluetooth
}
enum ICDeviceTypeMask : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case camera
  case scanner
}
enum ICDeviceLocationTypeMask : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case local
  case shared
  case bonjour
  case bluetooth
  case remote
}
let ICTransportTypeUSB: String
let ICTransportTypeFireWire: String
let ICTransportTypeBluetooth: String
let ICTransportTypeTCPIP: String
let ICTransportTypeMassStorage: String
let ICDeviceLocationDescriptionUSB: String
let ICDeviceLocationDescriptionFireWire: String
let ICDeviceLocationDescriptionBluetooth: String
let ICDeviceLocationDescriptionMassStorage: String
let ICButtonTypeScan: String
let ICButtonTypeMail: String
let ICButtonTypeCopy: String
let ICButtonTypeWeb: String
let ICButtonTypePrint: String
let ICButtonTypeTransfer: String
let ICStatusNotificationKey: String
let ICStatusCodeKey: String
let ICLocalizedStatusNotificationKey: String
let ICDeviceCanEjectOrDisconnect: String
protocol ICDeviceDelegate : NSObjectProtocol {
  func didRemove(_ device: ICDevice)
  optional func device(_ device: ICDevice, didOpenSessionWithError error: NSError?)
  optional func deviceDidBecomeReady(_ device: ICDevice)
  optional func device(_ device: ICDevice, didCloseSessionWithError error: NSError?)
  optional func deviceDidChangeName(_ device: ICDevice)
  optional func deviceDidChangeSharingState(_ device: ICDevice)
  optional func device(_ device: ICDevice, didReceiveStatusInformation status: [String : AnyObject])
  optional func device(_ device: ICDevice, didEncounterError error: NSError?)
  optional func device(_ device: ICDevice, didReceiveButtonPress buttonType: String)
  optional func device(_ device: ICDevice, didReceiveCustomNotification notification: [String : AnyObject], data data: NSData)
}
class ICDevice : NSObject {
  unowned(unsafe) var delegate: @sil_unmanaged ICDeviceDelegate?
  var type: ICDeviceType { get }
  var name: String? { get }
  var icon: CGImage? { get }
  var capabilities: [String] { get }
  var modulePath: String { get }
  var moduleVersion: String { get }
  var moduleExecutableArchitecture: Int32 { get }
  var isRemote: Bool { get }
  var isShared: Bool { get }
  var hasConfigurableWiFiInterface: Bool { get }
  var transportType: String { get }
  var usbLocationID: Int32 { get }
  var usbProductID: Int32 { get }
  var usbVendorID: Int32 { get }
  var fwGUID: Int64 { get }
  var serialNumberString: String? { get }
  var locationDescription: String? { get }
  var hasOpenSession: Bool { get }
  var uuidString: String? { get }
  var persistentIDString: String? { get }
  var buttonPressed: String { get }
  var autolaunchApplicationPath: String?
  var userData: NSMutableDictionary? { get }
  func requestOpenSession()
  func requestCloseSession()
  func requestYield()
  func requestSendMessage(_ messageCode: UInt32, outData data: NSData, maxReturnedDataSize maxReturnedDataSize: UInt32, sendMessageDelegate sendMessageDelegate: AnyObject, didSendMessageSelector selector: Selector, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func requestEjectOrDisconnect()
}
