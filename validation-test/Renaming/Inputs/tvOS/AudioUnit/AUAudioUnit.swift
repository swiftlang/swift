
typealias AUAudioUnitStatus = OSStatus
typealias AUEventSampleTime = Int64
var AUEventSampleTimeImmediate: AUEventSampleTime { get }
typealias AUAudioFrameCount = UInt32
typealias AUAudioChannelCount = UInt32
enum AUAudioUnitBusType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case input
  case output
}
typealias AURenderPullInputBlock = (UnsafeMutablePointer<AudioUnitRenderActionFlags>, UnsafePointer<AudioTimeStamp>, AUAudioFrameCount, Int, UnsafeMutablePointer<AudioBufferList>) -> AUAudioUnitStatus
typealias AURenderBlock = (UnsafeMutablePointer<AudioUnitRenderActionFlags>, UnsafePointer<AudioTimeStamp>, AUAudioFrameCount, Int, UnsafeMutablePointer<AudioBufferList>, AURenderPullInputBlock?) -> AUAudioUnitStatus
typealias AURenderObserver = (AudioUnitRenderActionFlags, UnsafePointer<AudioTimeStamp>, AUAudioFrameCount, Int) -> Void
typealias AUScheduleParameterBlock = (AUEventSampleTime, AUAudioFrameCount, AUParameterAddress, AUValue) -> Void
typealias AUScheduleMIDIEventBlock = (AUEventSampleTime, UInt8, Int, UnsafePointer<UInt8>) -> Void
typealias AUHostMusicalContextBlock = (UnsafeMutablePointer<Double>?, UnsafeMutablePointer<Double>?, UnsafeMutablePointer<Int>?, UnsafeMutablePointer<Double>?, UnsafeMutablePointer<Int>?, UnsafeMutablePointer<Double>?) -> Bool
struct AUHostTransportStateFlags : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var changed: AUHostTransportStateFlags { get }
  static var moving: AUHostTransportStateFlags { get }
  static var recording: AUHostTransportStateFlags { get }
  static var cycling: AUHostTransportStateFlags { get }
}
typealias AUHostTransportStateBlock = (UnsafeMutablePointer<AUHostTransportStateFlags>?, UnsafeMutablePointer<Double>?, UnsafeMutablePointer<Double>?, UnsafeMutablePointer<Double>?) -> Bool
@available(tvOS 9.0, *)
class AUAudioUnit : NSObject {
  init(componentDescription componentDescription: AudioComponentDescription, options options: AudioComponentInstantiationOptions = []) throws
  convenience init(componentDescription componentDescription: AudioComponentDescription) throws
  class func instantiate(with componentDescription: AudioComponentDescription, options options: AudioComponentInstantiationOptions = [], completionHandler completionHandler: (AUAudioUnit?, NSError?) -> Void)
  var componentDescription: AudioComponentDescription { get }
  var component: AudioComponent { get }
  var componentName: String? { get }
  var audioUnitName: String? { get }
  var manufacturerName: String? { get }
  var componentVersion: UInt32 { get }
  func allocateRenderResources() throws
  func deallocateRenderResources()
  var renderResourcesAllocated: Bool { get }
  func reset()
  var inputBusses: AUAudioUnitBusArray { get }
  var outputBusses: AUAudioUnitBusArray { get }
  var renderBlock: AURenderBlock { get }
  var scheduleParameterBlock: AUScheduleParameterBlock { get }
  @discardableResult
  func token(byAddingRenderObserver observer: AURenderObserver) -> Int
  func removeRenderObserver(_ token: Int)
  var maximumFramesToRender: AUAudioFrameCount
  var parameterTree: AUParameterTree? { get }
  @discardableResult
  func parametersForOverview(withCount count: Int) -> [NSNumber]
  var allParameterValues: Bool { get }
  var isMusicDeviceOrEffect: Bool { get }
  var virtualMIDICableCount: Int { get }
  var scheduleMIDIEventBlock: AUScheduleMIDIEventBlock? { get }
  var fullState: [String : AnyObject]?
  var fullStateForDocument: [String : AnyObject]?
  var factoryPresets: [AUAudioUnitPreset]? { get }
  var currentPreset: AUAudioUnitPreset?
  var latency: NSTimeInterval { get }
  var tailTime: NSTimeInterval { get }
  var renderQuality: Int
  var shouldBypassEffect: Bool
  var canProcessInPlace: Bool { get }
  var isRenderingOffline: Bool
  var channelCapabilities: [NSNumber]? { get }
  var musicalContextBlock: AUHostMusicalContextBlock?
  var transportStateBlock: AUHostTransportStateBlock?
  var contextName: String?
}
typealias AUInputHandler = (UnsafeMutablePointer<AudioUnitRenderActionFlags>, UnsafePointer<AudioTimeStamp>, AUAudioFrameCount, Int) -> Void
extension AUAudioUnit {
  var canPerformInput: Bool { get }
  var canPerformOutput: Bool { get }
  var isInputEnabled: Bool
  var isOutputEnabled: Bool
  var outputProvider: AURenderPullInputBlock?
  var inputHandler: AUInputHandler?
  func startHardware() throws
  func stopHardware()
}
@available(tvOS 9.0, *)
class AUAudioUnitBusArray : NSObject, NSFastEnumeration {
  init(audioUnit owner: AUAudioUnit, busType busType: AUAudioUnitBusType, busses busArray: [AUAudioUnitBus])
  convenience init(audioUnit owner: AUAudioUnit, busType busType: AUAudioUnitBusType)
  var count: Int { get }
  subscript(_ index: Int) -> AUAudioUnitBus { get }
  var isCountChangeable: Bool { get }
  func setBusCount(_ count: Int) throws
  func addObserver(toAllBusses observer: NSObject, forKeyPath keyPath: String, options options: NSKeyValueObservingOptions = [], context context: UnsafeMutablePointer<Void>?)
  func removeObserver(fromAllBusses observer: NSObject, forKeyPath keyPath: String, context context: UnsafeMutablePointer<Void>?)
  unowned(unsafe) var ownerAudioUnit: @sil_unmanaged AUAudioUnit { get }
  var busType: AUAudioUnitBusType { get }
}
@available(tvOS 9.0, *)
class AUAudioUnitBus : NSObject {
  var isEnabled: Bool
  var name: String?
  var index: Int { get }
  var busType: AUAudioUnitBusType { get }
  unowned(unsafe) var ownerAudioUnit: @sil_unmanaged AUAudioUnit { get }
  var supportedChannelLayoutTags: [NSNumber]? { get }
  var contextPresentationLatency: NSTimeInterval
}
@available(tvOS 9.0, *)
class AUAudioUnitPreset : NSObject, NSSecureCoding {
  var number: Int
  var name: String
}
