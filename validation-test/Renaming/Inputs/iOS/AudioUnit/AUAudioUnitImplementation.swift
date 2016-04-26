
enum AURenderEventType : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case parameter
  case parameterRamp
  case MIDI
  case midiSysEx
}
struct AURenderEventHeader {
  var next: UnsafeMutablePointer<AURenderEvent>?
  var eventSampleTime: AUEventSampleTime
  var eventType: AURenderEventType
  var reserved: UInt8
  init()
  init(next next: UnsafeMutablePointer<AURenderEvent>?, eventSampleTime eventSampleTime: AUEventSampleTime, eventType eventType: AURenderEventType, reserved reserved: UInt8)
}
struct AUParameterEvent {
  var next: UnsafeMutablePointer<AURenderEvent>?
  var eventSampleTime: AUEventSampleTime
  var eventType: AURenderEventType
  var reserved: (UInt8, UInt8, UInt8)
  var rampDurationSampleFrames: AUAudioFrameCount
  var parameterAddress: AUParameterAddress
  var value: AUValue
  init()
  init(next next: UnsafeMutablePointer<AURenderEvent>?, eventSampleTime eventSampleTime: AUEventSampleTime, eventType eventType: AURenderEventType, reserved reserved: (UInt8, UInt8, UInt8), rampDurationSampleFrames rampDurationSampleFrames: AUAudioFrameCount, parameterAddress parameterAddress: AUParameterAddress, value value: AUValue)
}
struct AUMIDIEvent {
  var next: UnsafeMutablePointer<AURenderEvent>?
  var eventSampleTime: AUEventSampleTime
  var eventType: AURenderEventType
  var reserved: UInt8
  var length: UInt16
  var cable: UInt8
  var data: (UInt8, UInt8, UInt8)
  init()
  init(next next: UnsafeMutablePointer<AURenderEvent>?, eventSampleTime eventSampleTime: AUEventSampleTime, eventType eventType: AURenderEventType, reserved reserved: UInt8, length length: UInt16, cable cable: UInt8, data data: (UInt8, UInt8, UInt8))
}
struct AURenderEvent {
  var head: AURenderEventHeader
  var parameter: AUParameterEvent
  var MIDI: AUMIDIEvent
  init(head head: AURenderEventHeader)
  init(parameter parameter: AUParameterEvent)
  init(MIDI MIDI: AUMIDIEvent)
  init()
}
typealias AUInternalRenderBlock = (UnsafeMutablePointer<AudioUnitRenderActionFlags>, UnsafePointer<AudioTimeStamp>, AUAudioFrameCount, Int, UnsafeMutablePointer<AudioBufferList>, UnsafePointer<AURenderEvent>?, AURenderPullInputBlock?) -> AUAudioUnitStatus
extension AUAudioUnit {
  class func registerSubclass(_ cls: AnyClass, as componentDescription: AudioComponentDescription, name name: String, version version: UInt32)
  var internalRenderBlock: AUInternalRenderBlock { get }
  func setRenderResourcesAllocated(_ flag: Bool)
}
extension AUAudioUnitBus {
  var supportedChannelCounts: [NSNumber]?
  var maximumChannelCount: AUAudioChannelCount
}
extension AUAudioUnitBusArray {
  func replaceBusses(_ busArray: [AUAudioUnitBus])
}
extension AUParameterTree {
  @discardableResult
  class func createParameter(withIdentifier identifier: String, name name: String, address address: AUParameterAddress, min min: AUValue, max max: AUValue, unit unit: AudioUnitParameterUnit, unitName unitName: String?, flags flags: AudioUnitParameterOptions = [], valueStrings valueStrings: [String]?, dependentParameters dependentParameters: [NSNumber]?) -> AUParameter
  @discardableResult
  class func createGroup(withIdentifier identifier: String, name name: String, children children: [AUParameterNode]) -> AUParameterGroup
  @discardableResult
  class func createGroupTemplate(_ children: [AUParameterNode]) -> AUParameterGroup
  @discardableResult
  class func createGroup(fromTemplate templateGroup: AUParameterGroup, identifier identifier: String, name name: String, addressOffset addressOffset: AUParameterAddress) -> AUParameterGroup
  @discardableResult
  class func createTree(withChildren children: [AUParameterNode]) -> AUParameterTree
}
typealias AUImplementorValueObserver = (AUParameter, AUValue) -> Void
typealias AUImplementorValueProvider = (AUParameter) -> AUValue
typealias AUImplementorStringFromValueCallback = (AUParameter, UnsafePointer<AUValue>?) -> String
typealias AUImplementorValueFromStringCallback = (AUParameter, String) -> AUValue
typealias AUImplementorDisplayNameWithLengthCallback = (AUParameterNode, Int) -> String
extension AUParameterNode {
  var implementorValueObserver: AUImplementorValueObserver
  var implementorValueProvider: AUImplementorValueProvider
  var implementorStringFromValueCallback: AUImplementorStringFromValueCallback
  var implementorValueFromStringCallback: AUImplementorValueFromStringCallback
  var implementorDisplayNameWithLengthCallback: AUImplementorDisplayNameWithLengthCallback
}
@available(iOS 9.0, *)
class AUAudioUnitV2Bridge : AUAudioUnit {
}
protocol AUAudioUnitFactory : NSExtensionRequestHandling {
  @available(iOS 9.0, *)
  @discardableResult
  func createAudioUnit(with desc: AudioComponentDescription) throws -> AUAudioUnit
}
