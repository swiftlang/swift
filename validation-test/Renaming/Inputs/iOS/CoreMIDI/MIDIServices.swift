
var kMIDIInvalidClient: OSStatus { get }
var kMIDIInvalidPort: OSStatus { get }
var kMIDIWrongEndpointType: OSStatus { get }
var kMIDINoConnection: OSStatus { get }
var kMIDIUnknownEndpoint: OSStatus { get }
var kMIDIUnknownProperty: OSStatus { get }
var kMIDIWrongPropertyType: OSStatus { get }
var kMIDINoCurrentSetup: OSStatus { get }
var kMIDIMessageSendErr: OSStatus { get }
var kMIDIServerStartErr: OSStatus { get }
var kMIDISetupFormatErr: OSStatus { get }
var kMIDIWrongThread: OSStatus { get }
var kMIDIObjectNotFound: OSStatus { get }
var kMIDIIDNotUnique: OSStatus { get }
var kMIDINotPermitted: OSStatus { get }
typealias MIDIObjectRef = UInt32
typealias MIDIClientRef = MIDIObjectRef
typealias MIDIPortRef = MIDIObjectRef
typealias MIDIDeviceRef = MIDIObjectRef
typealias MIDIEntityRef = MIDIObjectRef
typealias MIDIEndpointRef = MIDIObjectRef
typealias MIDITimeStamp = UInt64
enum MIDIObjectType : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case other
  case device
  case entity
  case source
  case destination
  case externalDevice
  case externalEntity
  case externalSource
  case externalDestination
}
let kMIDIObjectType_ExternalMask: MIDIObjectType
typealias MIDIUniqueID = Int32
var kMIDIInvalidUniqueID: MIDIUniqueID { get }
typealias MIDINotifyProc = @convention(c) (UnsafePointer<MIDINotification>, UnsafeMutablePointer<Void>?) -> Void
typealias MIDINotifyBlock = (UnsafePointer<MIDINotification>) -> Void
typealias MIDIReadProc = @convention(c) (UnsafePointer<MIDIPacketList>, UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?) -> Void
typealias MIDIReadBlock = (UnsafePointer<MIDIPacketList>, UnsafeMutablePointer<Void>?) -> Void
typealias MIDICompletionProc = @convention(c) (UnsafeMutablePointer<MIDISysexSendRequest>) -> Void
struct MIDIPacket {
  var timeStamp: MIDITimeStamp
  var length: UInt16
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(timeStamp timeStamp: MIDITimeStamp, length length: UInt16, data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct MIDIPacketList {
  var numPackets: UInt32
  var packet: (MIDIPacket)
  init()
  init(numPackets numPackets: UInt32, packet packet: (MIDIPacket))
}
struct MIDISysexSendRequest {
  var destination: MIDIEndpointRef
  var data: UnsafePointer<UInt8>
  var bytesToSend: UInt32
  var complete: DarwinBoolean
  var reserved: (UInt8, UInt8, UInt8)
  var completionProc: MIDICompletionProc
  var completionRefCon: UnsafeMutablePointer<Void>?
}
enum MIDINotificationMessageID : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case msgSetupChanged
  case msgObjectAdded
  case msgObjectRemoved
  case msgPropertyChanged
  case msgThruConnectionsChanged
  case msgSerialPortOwnerChanged
  case msgIOError
}
struct MIDINotification {
  var messageID: MIDINotificationMessageID
  var messageSize: UInt32
  init()
  init(messageID messageID: MIDINotificationMessageID, messageSize messageSize: UInt32)
}
struct MIDIObjectAddRemoveNotification {
  var messageID: MIDINotificationMessageID
  var messageSize: UInt32
  var parent: MIDIObjectRef
  var parentType: MIDIObjectType
  var child: MIDIObjectRef
  var childType: MIDIObjectType
  init()
  init(messageID messageID: MIDINotificationMessageID, messageSize messageSize: UInt32, parent parent: MIDIObjectRef, parentType parentType: MIDIObjectType, child child: MIDIObjectRef, childType childType: MIDIObjectType)
}
struct MIDIObjectPropertyChangeNotification {
  var messageID: MIDINotificationMessageID
  var messageSize: UInt32
  var object: MIDIObjectRef
  var objectType: MIDIObjectType
  var propertyName: Unmanaged<CFString>
}
struct MIDIIOErrorNotification {
  var messageID: MIDINotificationMessageID
  var messageSize: UInt32
  var driverDevice: MIDIDeviceRef
  var errorCode: OSStatus
  init()
  init(messageID messageID: MIDINotificationMessageID, messageSize messageSize: UInt32, driverDevice driverDevice: MIDIDeviceRef, errorCode errorCode: OSStatus)
}
@available(iOS 4.2, *)
let kMIDIPropertyName: CFString
@available(iOS 4.2, *)
let kMIDIPropertyManufacturer: CFString
@available(iOS 4.2, *)
let kMIDIPropertyModel: CFString
@available(iOS 4.2, *)
let kMIDIPropertyUniqueID: CFString
@available(iOS 4.2, *)
let kMIDIPropertyDeviceID: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceiveChannels: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitChannels: CFString
@available(iOS 4.2, *)
let kMIDIPropertyMaxSysExSpeed: CFString
@available(iOS 4.2, *)
let kMIDIPropertyAdvanceScheduleTimeMuSec: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsEmbeddedEntity: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsBroadcast: CFString
@available(iOS 4.2, *)
let kMIDIPropertySingleRealtimeEntity: CFString
@available(iOS 4.2, *)
let kMIDIPropertyConnectionUniqueID: CFString
@available(iOS 4.2, *)
let kMIDIPropertyOffline: CFString
@available(iOS 4.2, *)
let kMIDIPropertyPrivate: CFString
@available(iOS 4.2, *)
let kMIDIPropertyDriverOwner: CFString
@available(iOS 4.2, *)
let kMIDIPropertyNameConfiguration: CFString
@available(iOS 4.2, *)
let kMIDIPropertyImage: CFString
@available(iOS 4.2, *)
let kMIDIPropertyDriverVersion: CFString
@available(iOS 4.2, *)
let kMIDIPropertySupportsGeneralMIDI: CFString
@available(iOS 4.2, *)
let kMIDIPropertySupportsMMC: CFString
@available(iOS 4.2, *)
let kMIDIPropertyCanRoute: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesClock: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesMTC: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesNotes: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesProgramChanges: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesBankSelectMSB: CFString
@available(iOS 4.2, *)
let kMIDIPropertyReceivesBankSelectLSB: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsClock: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsMTC: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsNotes: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsProgramChanges: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsBankSelectMSB: CFString
@available(iOS 4.2, *)
let kMIDIPropertyTransmitsBankSelectLSB: CFString
@available(iOS 4.2, *)
let kMIDIPropertyPanDisruptsStereo: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsSampler: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsDrumMachine: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsMixer: CFString
@available(iOS 4.2, *)
let kMIDIPropertyIsEffectUnit: CFString
@available(iOS 4.2, *)
let kMIDIPropertyMaxReceiveChannels: CFString
@available(iOS 4.2, *)
let kMIDIPropertyMaxTransmitChannels: CFString
@available(iOS 4.2, *)
let kMIDIPropertyDriverDeviceEditorApp: CFString
@available(iOS 4.2, *)
let kMIDIPropertySupportsShowControl: CFString
@available(iOS 4.2, *)
let kMIDIPropertyDisplayName: CFString
@available(iOS 4.2, *)
@discardableResult
func MIDIClientCreate(_ name: CFString, _ notifyProc: MIDINotifyProc?, _ notifyRefCon: UnsafeMutablePointer<Void>?, _ outClient: UnsafeMutablePointer<MIDIClientRef>) -> OSStatus
@available(iOS 9.0, *)
@discardableResult
func MIDIClientCreateWithBlock(_ name: CFString, _ outClient: UnsafeMutablePointer<MIDIClientRef>, _ notifyBlock: MIDINotifyBlock?) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIClientDispose(_ client: MIDIClientRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIInputPortCreate(_ client: MIDIClientRef, _ portName: CFString, _ readProc: MIDIReadProc, _ refCon: UnsafeMutablePointer<Void>?, _ outPort: UnsafeMutablePointer<MIDIPortRef>) -> OSStatus
@available(iOS 9.0, *)
@discardableResult
func MIDIInputPortCreateWithBlock(_ client: MIDIClientRef, _ portName: CFString, _ outPort: UnsafeMutablePointer<MIDIPortRef>, _ readBlock: MIDIReadBlock) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIOutputPortCreate(_ client: MIDIClientRef, _ portName: CFString, _ outPort: UnsafeMutablePointer<MIDIPortRef>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIPortDispose(_ port: MIDIPortRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIPortConnectSource(_ port: MIDIPortRef, _ source: MIDIEndpointRef, _ connRefCon: UnsafeMutablePointer<Void>?) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIPortDisconnectSource(_ port: MIDIPortRef, _ source: MIDIEndpointRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIGetNumberOfDevices() -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIGetDevice(_ deviceIndex0: Int) -> MIDIDeviceRef
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceGetNumberOfEntities(_ device: MIDIDeviceRef) -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIDeviceGetEntity(_ device: MIDIDeviceRef, _ entityIndex0: Int) -> MIDIEntityRef
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityGetNumberOfSources(_ entity: MIDIEntityRef) -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityGetSource(_ entity: MIDIEntityRef, _ sourceIndex0: Int) -> MIDIEndpointRef
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityGetNumberOfDestinations(_ entity: MIDIEntityRef) -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityGetDestination(_ entity: MIDIEntityRef, _ destIndex0: Int) -> MIDIEndpointRef
@available(iOS 4.2, *)
@discardableResult
func MIDIEntityGetDevice(_ inEntity: MIDIEntityRef, _ outDevice: UnsafeMutablePointer<MIDIDeviceRef>?) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIGetNumberOfSources() -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIGetSource(_ sourceIndex0: Int) -> MIDIEndpointRef
@available(iOS 4.2, *)
@discardableResult
func MIDIGetNumberOfDestinations() -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIGetDestination(_ destIndex0: Int) -> MIDIEndpointRef
@available(iOS 4.2, *)
@discardableResult
func MIDIEndpointGetEntity(_ inEndpoint: MIDIEndpointRef, _ outEntity: UnsafeMutablePointer<MIDIEntityRef>?) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIDestinationCreate(_ client: MIDIClientRef, _ name: CFString, _ readProc: MIDIReadProc, _ refCon: UnsafeMutablePointer<Void>?, _ outDest: UnsafeMutablePointer<MIDIEndpointRef>) -> OSStatus
@available(iOS 9.0, *)
@discardableResult
func MIDIDestinationCreateWithBlock(_ client: MIDIClientRef, _ name: CFString, _ outDest: UnsafeMutablePointer<MIDIEndpointRef>, _ readBlock: MIDIReadBlock) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISourceCreate(_ client: MIDIClientRef, _ name: CFString, _ outSrc: UnsafeMutablePointer<MIDIEndpointRef>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIEndpointDispose(_ endpt: MIDIEndpointRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIGetNumberOfExternalDevices() -> Int
@available(iOS 4.2, *)
@discardableResult
func MIDIGetExternalDevice(_ deviceIndex0: Int) -> MIDIDeviceRef
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectGetIntegerProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outValue: UnsafeMutablePointer<Int32>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectSetIntegerProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ value: Int32) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectGetStringProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ str: UnsafeMutablePointer<Unmanaged<CFString>?>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectSetStringProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ str: CFString) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectGetDataProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outData: UnsafeMutablePointer<Unmanaged<CFData>?>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectSetDataProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ data: CFData) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectGetDictionaryProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outDict: UnsafeMutablePointer<Unmanaged<CFDictionary>?>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectSetDictionaryProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ dict: CFDictionary) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectGetProperties(_ obj: MIDIObjectRef, _ outProperties: UnsafeMutablePointer<Unmanaged<CFPropertyList>?>, _ deep: Bool) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectRemoveProperty(_ obj: MIDIObjectRef, _ propertyID: CFString) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIObjectFindByUniqueID(_ inUniqueID: MIDIUniqueID, _ outObject: UnsafeMutablePointer<MIDIObjectRef>, _ outObjectType: UnsafeMutablePointer<MIDIObjectType>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISend(_ port: MIDIPortRef, _ dest: MIDIEndpointRef, _ pktlist: UnsafePointer<MIDIPacketList>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDISendSysex(_ request: UnsafeMutablePointer<MIDISysexSendRequest>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIReceived(_ src: MIDIEndpointRef, _ pktlist: UnsafePointer<MIDIPacketList>) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIFlushOutput(_ dest: MIDIEndpointRef) -> OSStatus
@available(iOS 4.2, *)
@discardableResult
func MIDIRestart() -> OSStatus
@discardableResult
func MIDIPacketNext(_ pkt: UnsafePointer<MIDIPacket>) -> UnsafeMutablePointer<MIDIPacket>
@available(iOS 4.2, *)
@discardableResult
func MIDIPacketListInit(_ pktlist: UnsafeMutablePointer<MIDIPacketList>) -> UnsafeMutablePointer<MIDIPacket>
@available(iOS 4.2, *)
@discardableResult
func MIDIPacketListAdd(_ pktlist: UnsafeMutablePointer<MIDIPacketList>, _ listSize: Int, _ curPacket: UnsafeMutablePointer<MIDIPacket>, _ time: MIDITimeStamp, _ nData: Int, _ data: UnsafePointer<UInt8>) -> UnsafeMutablePointer<MIDIPacket>
