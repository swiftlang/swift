
@available(iOS 6.0, *)
struct AVAudioSessionInterruptionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var shouldResume: AVAudioSessionInterruptionOptions { get }
}
@available(iOS 6.0, *)
struct AVAudioSessionSetActiveOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var notifyOthersOnDeactivation: AVAudioSessionSetActiveOptions { get }
}
@available(iOS 6.0, *)
enum AVAudioSessionPortOverride : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case speaker
}
@available(iOS 6.0, *)
enum AVAudioSessionRouteChangeReason : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unknown
  case newDeviceAvailable
  case oldDeviceUnavailable
  case categoryChange
  case override
  case wakeFromSleep
  case noSuitableRouteForCategory
  @available(iOS 7.0, *)
  case routeConfigurationChange
}
@available(iOS 6.0, *)
struct AVAudioSessionCategoryOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var mixWithOthers: AVAudioSessionCategoryOptions { get }
  static var duckOthers: AVAudioSessionCategoryOptions { get }
  static var allowBluetooth: AVAudioSessionCategoryOptions { get }
  static var defaultToSpeaker: AVAudioSessionCategoryOptions { get }
  @available(iOS 9.0, *)
  static var interruptSpokenAudioAndMixWithOthers: AVAudioSessionCategoryOptions { get }
}
@available(iOS 6.0, *)
enum AVAudioSessionInterruptionType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case began
  case ended
}
@available(iOS 8.0, *)
enum AVAudioSessionSilenceSecondaryAudioHintType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case begin
  case end
}
@available(iOS 8.0, *)
struct AVAudioSessionRecordPermission : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var undetermined: AVAudioSessionRecordPermission { get }
  static var denied: AVAudioSessionRecordPermission { get }
  static var granted: AVAudioSessionRecordPermission { get }
}
@available(iOS 7.0, *)
enum AVAudioSessionErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case codeNone
  case codeMediaServicesFailed
  case codeIsBusy
  case codeIncompatibleCategory
  case codeCannotInterruptOthers
  case codeMissingEntitlement
  case codeSiriIsRecording
  case codeCannotStartPlaying
  case codeCannotStartRecording
  case codeBadParam
  case insufficientPriority
  case codeResourceNotAvailable
  case codeUnspecified
}
@available(iOS 3.0, *)
class AVAudioSession : NSObject {
  @discardableResult
  class func sharedInstance() -> AVAudioSession
  func setActive(_ active: Bool) throws
  @available(iOS 6.0, *)
  func setActive(_ active: Bool, with options: AVAudioSessionSetActiveOptions = []) throws
  @available(iOS 9.0, *)
  var availableCategories: [String] { get }
  func setCategory(_ category: String) throws
  @available(iOS 6.0, *)
  func setCategory(_ category: String, with options: AVAudioSessionCategoryOptions = []) throws
  var category: String { get }
  @available(iOS 8.0, *)
  @discardableResult
  func recordPermission() -> AVAudioSessionRecordPermission
  @available(iOS 7.0, *)
  func requestRecordPermission(_ response: PermissionBlock)
  @available(iOS 6.0, *)
  var categoryOptions: AVAudioSessionCategoryOptions { get }
  @available(iOS 9.0, *)
  var availableModes: [String] { get }
  @available(iOS 5.0, *)
  func setMode(_ mode: String) throws
  @available(iOS 5.0, *)
  var mode: String { get }
  @available(iOS 6.0, *)
  func overrideOutputAudioPort(_ portOverride: AVAudioSessionPortOverride) throws
  @available(iOS 6.0, *)
  var isOtherAudioPlaying: Bool { get }
  @available(iOS 8.0, *)
  var secondaryAudioShouldBeSilencedHint: Bool { get }
  @available(iOS 6.0, *)
  var currentRoute: AVAudioSessionRouteDescription { get }
  @available(iOS 7.0, *)
  func setPreferredInput(_ inPort: AVAudioSessionPortDescription?) throws
  @available(iOS 7.0, *)
  var preferredInput: AVAudioSessionPortDescription? { get }
  @available(iOS 7.0, *)
  var availableInputs: [AVAudioSessionPortDescription]? { get }
}
typealias PermissionBlock = (Bool) -> Void
extension AVAudioSession {
  @available(iOS 6.0, *)
  func setPreferredSampleRate(_ sampleRate: Double) throws
  @available(iOS 6.0, *)
  var preferredSampleRate: Double { get }
  func setPreferredIOBufferDuration(_ duration: NSTimeInterval) throws
  var preferredIOBufferDuration: NSTimeInterval { get }
  @available(iOS 7.0, *)
  func setPreferredInputNumberOfChannels(_ count: Int) throws
  @available(iOS 7.0, *)
  var preferredInputNumberOfChannels: Int { get }
  @available(iOS 7.0, *)
  func setPreferredOutputNumberOfChannels(_ count: Int) throws
  @available(iOS 7.0, *)
  var preferredOutputNumberOfChannels: Int { get }
  @available(iOS 7.0, *)
  var maximumInputNumberOfChannels: Int { get }
  @available(iOS 7.0, *)
  var maximumOutputNumberOfChannels: Int { get }
  @available(iOS 6.0, *)
  func setInputGain(_ gain: Float) throws
  @available(iOS 6.0, *)
  var inputGain: Float { get }
  @available(iOS 6.0, *)
  var isInputGainSettable: Bool { get }
  @available(iOS 6.0, *)
  var isInputAvailable: Bool { get }
  @available(iOS 6.0, *)
  var inputDataSources: [AVAudioSessionDataSourceDescription]? { get }
  @available(iOS 6.0, *)
  var inputDataSource: AVAudioSessionDataSourceDescription? { get }
  @available(iOS 6.0, *)
  func setInputDataSource(_ dataSource: AVAudioSessionDataSourceDescription?) throws
  @available(iOS 6.0, *)
  var outputDataSources: [AVAudioSessionDataSourceDescription]? { get }
  @available(iOS 6.0, *)
  var outputDataSource: AVAudioSessionDataSourceDescription? { get }
  @available(iOS 6.0, *)
  func setOutputDataSource(_ dataSource: AVAudioSessionDataSourceDescription?) throws
  @available(iOS 6.0, *)
  var sampleRate: Double { get }
  @available(iOS 6.0, *)
  var inputNumberOfChannels: Int { get }
  @available(iOS 6.0, *)
  var outputNumberOfChannels: Int { get }
  @available(iOS 6.0, *)
  var outputVolume: Float { get }
  @available(iOS 6.0, *)
  var inputLatency: NSTimeInterval { get }
  @available(iOS 6.0, *)
  var outputLatency: NSTimeInterval { get }
  @available(iOS 6.0, *)
  var ioBufferDuration: NSTimeInterval { get }
}
extension AVAudioSession {
}
@available(iOS 6.0, *)
let AVAudioSessionInterruptionNotification: String
@available(iOS 6.0, *)
let AVAudioSessionRouteChangeNotification: String
@available(iOS 7.0, *)
let AVAudioSessionMediaServicesWereLostNotification: String
@available(iOS 6.0, *)
let AVAudioSessionMediaServicesWereResetNotification: String
@available(iOS 8.0, *)
let AVAudioSessionSilenceSecondaryAudioHintNotification: String
@available(iOS 6.0, *)
let AVAudioSessionInterruptionTypeKey: String
@available(iOS 6.0, *)
let AVAudioSessionInterruptionOptionKey: String
@available(iOS 6.0, *)
let AVAudioSessionRouteChangeReasonKey: String
@available(iOS 6.0, *)
let AVAudioSessionRouteChangePreviousRouteKey: String
@available(iOS 8.0, *)
let AVAudioSessionSilenceSecondaryAudioHintTypeKey: String
let AVAudioSessionCategoryAmbient: String
let AVAudioSessionCategorySoloAmbient: String
let AVAudioSessionCategoryPlayback: String
let AVAudioSessionCategoryRecord: String
let AVAudioSessionCategoryPlayAndRecord: String
let AVAudioSessionCategoryAudioProcessing: String
@available(iOS 6.0, *)
let AVAudioSessionCategoryMultiRoute: String
@available(iOS 5.0, *)
let AVAudioSessionModeDefault: String
@available(iOS 5.0, *)
let AVAudioSessionModeVoiceChat: String
@available(iOS 5.0, *)
let AVAudioSessionModeGameChat: String
@available(iOS 5.0, *)
let AVAudioSessionModeVideoRecording: String
@available(iOS 5.0, *)
let AVAudioSessionModeMeasurement: String
@available(iOS 6.0, *)
let AVAudioSessionModeMoviePlayback: String
@available(iOS 7.0, *)
let AVAudioSessionModeVideoChat: String
@available(iOS 9.0, *)
let AVAudioSessionModeSpokenAudio: String
@available(iOS 6.0, *)
let AVAudioSessionPortLineIn: String
@available(iOS 6.0, *)
let AVAudioSessionPortBuiltInMic: String
@available(iOS 6.0, *)
let AVAudioSessionPortHeadsetMic: String
@available(iOS 6.0, *)
let AVAudioSessionPortLineOut: String
@available(iOS 6.0, *)
let AVAudioSessionPortHeadphones: String
@available(iOS 6.0, *)
let AVAudioSessionPortBluetoothA2DP: String
@available(iOS 6.0, *)
let AVAudioSessionPortBuiltInReceiver: String
@available(iOS 6.0, *)
let AVAudioSessionPortBuiltInSpeaker: String
@available(iOS 6.0, *)
let AVAudioSessionPortHDMI: String
@available(iOS 6.0, *)
let AVAudioSessionPortAirPlay: String
@available(iOS 7.0, *)
let AVAudioSessionPortBluetoothLE: String
@available(iOS 6.0, *)
let AVAudioSessionPortBluetoothHFP: String
@available(iOS 6.0, *)
let AVAudioSessionPortUSBAudio: String
@available(iOS 7.0, *)
let AVAudioSessionPortCarAudio: String
@available(iOS 7.0, *)
let AVAudioSessionLocationUpper: String
@available(iOS 7.0, *)
let AVAudioSessionLocationLower: String
@available(iOS 7.0, *)
let AVAudioSessionOrientationTop: String
@available(iOS 7.0, *)
let AVAudioSessionOrientationBottom: String
@available(iOS 7.0, *)
let AVAudioSessionOrientationFront: String
@available(iOS 7.0, *)
let AVAudioSessionOrientationBack: String
@available(iOS 8.0, *)
let AVAudioSessionOrientationLeft: String
@available(iOS 8.0, *)
let AVAudioSessionOrientationRight: String
@available(iOS 7.0, *)
let AVAudioSessionPolarPatternOmnidirectional: String
@available(iOS 7.0, *)
let AVAudioSessionPolarPatternCardioid: String
@available(iOS 7.0, *)
let AVAudioSessionPolarPatternSubcardioid: String
@available(iOS 6.0, *)
class AVAudioSessionChannelDescription : NSObject {
  var channelName: String { get }
  var owningPortUID: String { get }
  var channelNumber: Int { get }
  var channelLabel: AudioChannelLabel { get }
}
@available(iOS 6.0, *)
class AVAudioSessionPortDescription : NSObject {
  var portType: String { get }
  var portName: String { get }
  var uid: String { get }
  var channels: [AVAudioSessionChannelDescription]? { get }
  @available(iOS 7.0, *)
  var dataSources: [AVAudioSessionDataSourceDescription]? { get }
  @available(iOS 7.0, *)
  var selectedDataSource: AVAudioSessionDataSourceDescription? { get }
  @available(iOS 7.0, *)
  var preferredDataSource: AVAudioSessionDataSourceDescription? { get }
  @available(iOS 7.0, *)
  func setPreferredDataSource(_ dataSource: AVAudioSessionDataSourceDescription?) throws
}
@available(iOS 6.0, *)
class AVAudioSessionRouteDescription : NSObject {
  var inputs: [AVAudioSessionPortDescription] { get }
  var outputs: [AVAudioSessionPortDescription] { get }
}
@available(iOS 6.0, *)
class AVAudioSessionDataSourceDescription : NSObject {
  var dataSourceID: NSNumber { get }
  var dataSourceName: String { get }
  @available(iOS 7.0, *)
  var location: String? { get }
  @available(iOS 7.0, *)
  var orientation: String? { get }
  @available(iOS 7.0, *)
  var supportedPolarPatterns: [String]? { get }
  @available(iOS 7.0, *)
  var selectedPolarPattern: String? { get }
  @available(iOS 7.0, *)
  var preferredPolarPattern: String? { get }
  @available(iOS 7.0, *)
  func setPreferredPolarPattern(_ pattern: String?) throws
}
protocol AVAudioSessionDelegate : NSObjectProtocol {
  optional func beginInterruption()
  @available(iOS 4.0, *)
  optional func endInterruption(withFlags flags: Int)
  optional func endInterruption()
  optional func inputIsAvailableChanged(_ isInputAvailable: Bool)
}
var AVAudioSessionInterruptionFlags_ShouldResume: Int { get }
var AVAudioSessionSetActiveFlags_NotifyOthersOnDeactivation: Int { get }
