
typealias MIDIThruConnectionRef = MIDIObjectRef
struct MIDIValueMap {
  var value: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(value value: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
enum MIDITransformType : UInt16 {
  init?(rawValue rawValue: UInt16)
  var rawValue: UInt16 { get }
  case none
  case filterOut
  case mapControl
  case add
  case scale
  case minValue
  case maxValue
  case mapValue
}
var kMIDIThruConnection_MaxEndpoints: Int { get }
enum MIDITransformControlType : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case controlType_7Bit
  case controlType_14Bit
  case controlType_7BitRPN
  case controlType_14BitRPN
  case controlType_7BitNRPN
  case controlType_14BitNRPN
}
struct MIDITransform {
  var transform: MIDITransformType
  var param: Int16
  init()
  init(transform transform: MIDITransformType, param param: Int16)
}
struct MIDIControlTransform {
  var controlType: MIDITransformControlType
  var remappedControlType: MIDITransformControlType
  var controlNumber: UInt16
  var transform: MIDITransformType
  var param: Int16
  init()
  init(controlType controlType: MIDITransformControlType, remappedControlType remappedControlType: MIDITransformControlType, controlNumber controlNumber: UInt16, transform transform: MIDITransformType, param param: Int16)
}
struct MIDIThruConnectionEndpoint {
  var endpointRef: MIDIEndpointRef
  var uniqueID: MIDIUniqueID
  init()
  init(endpointRef endpointRef: MIDIEndpointRef, uniqueID uniqueID: MIDIUniqueID)
}
struct MIDIThruConnectionParams {
  var version: UInt32
  var numSources: UInt32
  var sources: (MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint)
  var numDestinations: UInt32
  var destinations: (MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint)
  var channelMap: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var lowVelocity: UInt8
  var highVelocity: UInt8
  var lowNote: UInt8
  var highNote: UInt8
  var noteNumber: MIDITransform
  var velocity: MIDITransform
  var keyPressure: MIDITransform
  var channelPressure: MIDITransform
  var programChange: MIDITransform
  var pitchBend: MIDITransform
  var filterOutSysEx: UInt8
  var filterOutMTC: UInt8
  var filterOutBeatClock: UInt8
  var filterOutTuneRequest: UInt8
  var reserved2: (UInt8, UInt8, UInt8)
  var filterOutAllControls: UInt8
  var numControlTransforms: UInt16
  var numMaps: UInt16
  var reserved3: (UInt16, UInt16, UInt16, UInt16)
  init()
  init(version version: UInt32, numSources numSources: UInt32, sources sources: (MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint), numDestinations numDestinations: UInt32, destinations destinations: (MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint, MIDIThruConnectionEndpoint), channelMap channelMap: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), lowVelocity lowVelocity: UInt8, highVelocity highVelocity: UInt8, lowNote lowNote: UInt8, highNote highNote: UInt8, noteNumber noteNumber: MIDITransform, velocity velocity: MIDITransform, keyPressure keyPressure: MIDITransform, channelPressure channelPressure: MIDITransform, programChange programChange: MIDITransform, pitchBend pitchBend: MIDITransform, filterOutSysEx filterOutSysEx: UInt8, filterOutMTC filterOutMTC: UInt8, filterOutBeatClock filterOutBeatClock: UInt8, filterOutTuneRequest filterOutTuneRequest: UInt8, reserved2 reserved2: (UInt8, UInt8, UInt8), filterOutAllControls filterOutAllControls: UInt8, numControlTransforms numControlTransforms: UInt16, numMaps numMaps: UInt16, reserved3 reserved3: (UInt16, UInt16, UInt16, UInt16))
}
@discardableResult
func MIDIThruConnectionParamsSize(_ ptr: UnsafePointer<MIDIThruConnectionParams>) -> Int
@available(OSX 10.2, *)
func MIDIThruConnectionParamsInitialize(_ inConnectionParams: UnsafeMutablePointer<MIDIThruConnectionParams>)
@available(OSX 10.2, *)
@discardableResult
func MIDIThruConnectionCreate(_ inPersistentOwnerID: CFString?, _ inConnectionParams: CFData, _ outConnection: UnsafeMutablePointer<MIDIThruConnectionRef>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIThruConnectionDispose(_ connection: MIDIThruConnectionRef) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIThruConnectionGetParams(_ connection: MIDIThruConnectionRef, _ outConnectionParams: UnsafeMutablePointer<Unmanaged<CFData>>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIThruConnectionSetParams(_ connection: MIDIThruConnectionRef, _ inConnectionParams: CFData) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func MIDIThruConnectionFind(_ inPersistentOwnerID: CFString, _ outConnectionList: UnsafeMutablePointer<Unmanaged<CFData>>) -> OSStatus
