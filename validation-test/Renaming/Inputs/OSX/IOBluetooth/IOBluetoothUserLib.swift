
class IOBluetoothObjectRef {
}
class IOBluetoothDeviceRef {
}
class IOBluetoothL2CAPChannelRef {
}
class IOBluetoothRFCOMMChannelRef {
}
class IOBluetoothSDPServiceRecordRef {
}
class IOBluetoothSDPUUIDRef {
}
class IOBluetoothSDPDataElementRef {
}
class IOBluetoothUserNotificationRef {
}
typealias IOBluetoothObjectID = UInt
typealias IOBluetoothDeviceSearchOptions = UInt32
struct IOBluetoothDeviceSearchOptionsBits : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kSearchOptionsNone: IOBluetoothDeviceSearchOptionsBits { get }
var kSearchOptionsAlwaysStartInquiry: IOBluetoothDeviceSearchOptionsBits { get }
var kSearchOptionsDiscardCachedResults: IOBluetoothDeviceSearchOptionsBits { get }
struct IOBluetoothDeviceSearchDeviceAttributes {
  var address: BluetoothDeviceAddress
  var name: BluetoothDeviceName
  var serviceClassMajor: BluetoothServiceClassMajor
  var deviceClassMajor: BluetoothDeviceClassMajor
  var deviceClassMinor: BluetoothDeviceClassMinor
  init()
  init(address address: BluetoothDeviceAddress, name name: BluetoothDeviceName, serviceClassMajor serviceClassMajor: BluetoothServiceClassMajor, deviceClassMajor deviceClassMajor: BluetoothDeviceClassMajor, deviceClassMinor deviceClassMinor: BluetoothDeviceClassMinor)
}
struct IOBluetoothDeviceSearchAttributes {
  var options: IOBluetoothDeviceSearchOptions
  var maxResults: IOItemCount
  var deviceAttributeCount: IOItemCount
  var attributeList: UnsafeMutablePointer<IOBluetoothDeviceSearchDeviceAttributes>!
  init()
  init(options options: IOBluetoothDeviceSearchOptions, maxResults maxResults: IOItemCount, deviceAttributeCount deviceAttributeCount: IOItemCount, attributeList attributeList: UnsafeMutablePointer<IOBluetoothDeviceSearchDeviceAttributes>!)
}
typealias IOBluetoothDeviceSearchTypes = UInt32
struct IOBluetoothDeviceSearchTypesBits : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kIOBluetoothDeviceSearchClassic: IOBluetoothDeviceSearchTypesBits { get }
var kIOBluetoothDeviceSearchLE: IOBluetoothDeviceSearchTypesBits { get }
func IOBluetoothIgnoreHIDDevice(_ device: IOBluetoothDeviceRef!)
func IOBluetoothRemoveIgnoredHIDDevice(_ device: IOBluetoothDeviceRef!)
struct IOBluetoothUserNotificationChannelDirection : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kIOBluetoothUserNotificationChannelDirectionAny: IOBluetoothUserNotificationChannelDirection { get }
var kIOBluetoothUserNotificationChannelDirectionIncoming: IOBluetoothUserNotificationChannelDirection { get }
var kIOBluetoothUserNotificationChannelDirectionOutgoing: IOBluetoothUserNotificationChannelDirection { get }
typealias IOBluetoothUserNotificationCallback = @convention(c) (UnsafeMutablePointer<Void>!, IOBluetoothUserNotificationRef!, IOBluetoothObjectRef!) -> Void
func IOBluetoothUserNotificationUnregister(_ notificationRef: IOBluetoothUserNotificationRef!)
@discardableResult
func IOBluetoothRegisterForDeviceConnectNotifications(_ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothDeviceRegisterForDisconnectNotification(_ inDevice: IOBluetoothDeviceRef!, _ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothRegisterForL2CAPChannelOpenNotifications(_ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothRegisterForFilteredL2CAPChannelOpenNotifications(_ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!, _ inPSM: BluetoothL2CAPPSM, _ inDirection: IOBluetoothUserNotificationChannelDirection) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothL2CAPChannelRegisterForChannelCloseNotification(_ channel: IOBluetoothL2CAPChannelRef!, _ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothRegisterForRFCOMMChannelOpenNotifications(_ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothRegisterForFilteredRFCOMMChannelOpenNotifications(_ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!, _ channelID: BluetoothRFCOMMChannelID, _ inDirection: IOBluetoothUserNotificationChannelDirection) -> Unmanaged<IOBluetoothUserNotificationRef>!
@discardableResult
func IOBluetoothRFCOMMChannelRegisterForChannelCloseNotification(_ inChannel: IOBluetoothRFCOMMChannelRef!, _ callback: IOBluetoothUserNotificationCallback!, _ inRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<IOBluetoothUserNotificationRef>!
