
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
@available(OSX 10.0, *)
let kMIDIPropertyName: CFString
@available(OSX 10.0, *)
let kMIDIPropertyManufacturer: CFString
@available(OSX 10.0, *)
let kMIDIPropertyModel: CFString
@available(OSX 10.0, *)
let kMIDIPropertyUniqueID: CFString
@available(OSX 10.0, *)
let kMIDIPropertyDeviceID: CFString
@available(OSX 10.0, *)
let kMIDIPropertyReceiveChannels: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitChannels: CFString
@available(OSX 10.0, *)
let kMIDIPropertyMaxSysExSpeed: CFString
@available(OSX 10.0, *)
let kMIDIPropertyAdvanceScheduleTimeMuSec: CFString
@available(OSX 10.1, *)
let kMIDIPropertyIsEmbeddedEntity: CFString
@available(OSX 10.2, *)
let kMIDIPropertyIsBroadcast: CFString
@available(OSX 10.2, *)
let kMIDIPropertySingleRealtimeEntity: CFString
@available(OSX 10.1, *)
let kMIDIPropertyConnectionUniqueID: CFString
@available(OSX 10.1, *)
let kMIDIPropertyOffline: CFString
@available(OSX 10.2, *)
let kMIDIPropertyPrivate: CFString
@available(OSX 10.1, *)
let kMIDIPropertyDriverOwner: CFString
@available(OSX 10.2, *)
let kMIDIPropertyNameConfiguration: CFString
@available(OSX 10.2, *)
let kMIDIPropertyImage: CFString
@available(OSX 10.2, *)
let kMIDIPropertyDriverVersion: CFString
@available(OSX 10.2, *)
let kMIDIPropertySupportsGeneralMIDI: CFString
@available(OSX 10.2, *)
let kMIDIPropertySupportsMMC: CFString
@available(OSX 10.0, *)
let kMIDIPropertyCanRoute: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesClock: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesMTC: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesNotes: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesProgramChanges: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesBankSelectMSB: CFString
@available(OSX 10.2, *)
let kMIDIPropertyReceivesBankSelectLSB: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsClock: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsMTC: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsNotes: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsProgramChanges: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsBankSelectMSB: CFString
@available(OSX 10.2, *)
let kMIDIPropertyTransmitsBankSelectLSB: CFString
@available(OSX 10.2, *)
let kMIDIPropertyPanDisruptsStereo: CFString
@available(OSX 10.2, *)
let kMIDIPropertyIsSampler: CFString
@available(OSX 10.2, *)
let kMIDIPropertyIsDrumMachine: CFString
@available(OSX 10.2, *)
let kMIDIPropertyIsMixer: CFString
@available(OSX 10.2, *)
let kMIDIPropertyIsEffectUnit: CFString
@available(OSX 10.2, *)
let kMIDIPropertyMaxReceiveChannels: CFString
@available(OSX 10.2, *)
let kMIDIPropertyMaxTransmitChannels: CFString
@available(OSX 10.3, *)
let kMIDIPropertyDriverDeviceEditorApp: CFString
@available(OSX 10.4, *)
let kMIDIPropertySupportsShowControl: CFString
@available(OSX 10.4, *)
let kMIDIPropertyDisplayName: CFString
@available(OSX 10.0, *)
@discardableResult
func MIDIClientCreate(_ name: CFString, _ notifyProc: MIDINotifyProc?, _ notifyRefCon: UnsafeMutablePointer<Void>?, _ outClient: UnsafeMutablePointer<MIDIClientRef>) -> OSStatus
@available(OSX 10.11, *)
@discardableResult
func MIDIClientCreateWithBlock(_ name: CFString, _ outClient: UnsafeMutablePointer<MIDIClientRef>, _ notifyBlock: MIDINotifyBlock?) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIClientDispose(_ client: MIDIClientRef) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIInputPortCreate(_ client: MIDIClientRef, _ portName: CFString, _ readProc: MIDIReadProc, _ refCon: UnsafeMutablePointer<Void>?, _ outPort: UnsafeMutablePointer<MIDIPortRef>) -> OSStatus
@available(OSX 10.11, *)
@discardableResult
func MIDIInputPortCreateWithBlock(_ client: MIDIClientRef, _ portName: CFString, _ outPort: UnsafeMutablePointer<MIDIPortRef>, _ readBlock: MIDIReadBlock) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIOutputPortCreate(_ client: MIDIClientRef, _ portName: CFString, _ outPort: UnsafeMutablePointer<MIDIPortRef>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIPortDispose(_ port: MIDIPortRef) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIPortConnectSource(_ port: MIDIPortRef, _ source: MIDIEndpointRef, _ connRefCon: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIPortDisconnectSource(_ port: MIDIPortRef, _ source: MIDIEndpointRef) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIGetNumberOfDevices() -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIGetDevice(_ deviceIndex0: Int) -> MIDIDeviceRef
@available(OSX 10.0, *)
@discardableResult
func MIDIDeviceGetNumberOfEntities(_ device: MIDIDeviceRef) -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIDeviceGetEntity(_ device: MIDIDeviceRef, _ entityIndex0: Int) -> MIDIEntityRef
@available(OSX 10.0, *)
@discardableResult
func MIDIEntityGetNumberOfSources(_ entity: MIDIEntityRef) -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIEntityGetSource(_ entity: MIDIEntityRef, _ sourceIndex0: Int) -> MIDIEndpointRef
@available(OSX 10.0, *)
@discardableResult
func MIDIEntityGetNumberOfDestinations(_ entity: MIDIEntityRef) -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIEntityGetDestination(_ entity: MIDIEntityRef, _ destIndex0: Int) -> MIDIEndpointRef
@available(OSX 10.2, *)
@discardableResult
func MIDIEntityGetDevice(_ inEntity: MIDIEntityRef, _ outDevice: UnsafeMutablePointer<MIDIDeviceRef>?) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIGetNumberOfSources() -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIGetSource(_ sourceIndex0: Int) -> MIDIEndpointRef
@available(OSX 10.0, *)
@discardableResult
func MIDIGetNumberOfDestinations() -> Int
@available(OSX 10.0, *)
@discardableResult
func MIDIGetDestination(_ destIndex0: Int) -> MIDIEndpointRef
@available(OSX 10.2, *)
@discardableResult
func MIDIEndpointGetEntity(_ inEndpoint: MIDIEndpointRef, _ outEntity: UnsafeMutablePointer<MIDIEntityRef>?) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIDestinationCreate(_ client: MIDIClientRef, _ name: CFString, _ readProc: MIDIReadProc, _ refCon: UnsafeMutablePointer<Void>?, _ outDest: UnsafeMutablePointer<MIDIEndpointRef>) -> OSStatus
@available(OSX 10.11, *)
@discardableResult
func MIDIDestinationCreateWithBlock(_ client: MIDIClientRef, _ name: CFString, _ outDest: UnsafeMutablePointer<MIDIEndpointRef>, _ readBlock: MIDIReadBlock) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDISourceCreate(_ client: MIDIClientRef, _ name: CFString, _ outSrc: UnsafeMutablePointer<MIDIEndpointRef>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIEndpointDispose(_ endpt: MIDIEndpointRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIGetNumberOfExternalDevices() -> Int
@available(OSX 10.1, *)
@discardableResult
func MIDIGetExternalDevice(_ deviceIndex0: Int) -> MIDIDeviceRef
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectGetIntegerProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outValue: UnsafeMutablePointer<Int32>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectSetIntegerProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ value: Int32) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectGetStringProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ str: UnsafeMutablePointer<Unmanaged<CFString>?>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectSetStringProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ str: CFString) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectGetDataProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outData: UnsafeMutablePointer<Unmanaged<CFData>?>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIObjectSetDataProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ data: CFData) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIObjectGetDictionaryProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ outDict: UnsafeMutablePointer<Unmanaged<CFDictionary>?>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIObjectSetDictionaryProperty(_ obj: MIDIObjectRef, _ propertyID: CFString, _ dict: CFDictionary) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIObjectGetProperties(_ obj: MIDIObjectRef, _ outProperties: UnsafeMutablePointer<Unmanaged<CFPropertyList>?>, _ deep: Bool) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIObjectRemoveProperty(_ obj: MIDIObjectRef, _ propertyID: CFString) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIObjectFindByUniqueID(_ inUniqueID: MIDIUniqueID, _ outObject: UnsafeMutablePointer<MIDIObjectRef>, _ outObjectType: UnsafeMutablePointer<MIDIObjectType>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDISend(_ port: MIDIPortRef, _ dest: MIDIEndpointRef, _ pktlist: UnsafePointer<MIDIPacketList>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDISendSysex(_ request: UnsafeMutablePointer<MIDISysexSendRequest>) -> OSStatus
@available(OSX 10.0, *)
@discardableResult
func MIDIReceived(_ src: MIDIEndpointRef, _ pktlist: UnsafePointer<MIDIPacketList>) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIFlushOutput(_ dest: MIDIEndpointRef) -> OSStatus
@available(OSX 10.1, *)
@discardableResult
func MIDIRestart() -> OSStatus
@discardableResult
func MIDIPacketNext(_ pkt: UnsafePointer<MIDIPacket>) -> UnsafeMutablePointer<MIDIPacket>
@available(OSX 10.0, *)
@discardableResult
func MIDIPacketListInit(_ pktlist: UnsafeMutablePointer<MIDIPacketList>) -> UnsafeMutablePointer<MIDIPacket>
@available(OSX 10.0, *)
@discardableResult
func MIDIPacketListAdd(_ pktlist: UnsafeMutablePointer<MIDIPacketList>, _ listSize: Int, _ curPacket: UnsafeMutablePointer<MIDIPacket>, _ time: MIDITimeStamp, _ nData: Int, _ data: UnsafePointer<UInt8>) -> UnsafeMutablePointer<MIDIPacket>
