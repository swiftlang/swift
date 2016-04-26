
var AVB_LEGACY_OBJC_RUNTIME: Int32 { get }
var AVB_MODERN_OBJC_RUNTIME: Int32 { get }
var AVBMACAddressSize: Int32 { get }
@available(OSX 10.8, *)
enum AVB17221ADPEntityCapabilities : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  @available(OSX 10.9, *)
  static var efuMode: AVB17221ADPEntityCapabilities { get }
  case addressAccessSupported
  case gatewayEntity
  case aemSupported
  case legacyAVC
  case associationIDSupported
  case associationIDValid
  case vendorUniqueSupported
  case classASupported
  case classBSupported
  @available(OSX 10.9, *)
  static var gptpSupported: AVB17221ADPEntityCapabilities { get }
  @available(OSX 10.9, *)
  case aemAuthenticationSupported
  @available(OSX 10.9, *)
  case aemAuthenticationRequired
  @available(OSX 10.9, *)
  case aemPersistentAcquireSupported
  @available(OSX 10.9, *)
  case aemIdenitifyControlIndexValid
  @available(OSX 10.9, *)
  case aemInterfaceIndexValid
  @available(OSX 10.9, *)
  case generalControllerIgnore
  @available(OSX 10.9, *)
  case entityNotReady
}
@available(OSX 10.8, *)
struct AVB17221ADPTalkerCapabilities : OptionSet {
  init(rawValue rawValue: UInt16)
  let rawValue: UInt16
  static var implemented: AVB17221ADPTalkerCapabilities { get }
  static var hasOtherSource: AVB17221ADPTalkerCapabilities { get }
  static var hasControlSource: AVB17221ADPTalkerCapabilities { get }
  static var hasMediaClockSource: AVB17221ADPTalkerCapabilities { get }
  static var hasSMPTESource: AVB17221ADPTalkerCapabilities { get }
  static var hasMIDISource: AVB17221ADPTalkerCapabilities { get }
  static var hasAudioSource: AVB17221ADPTalkerCapabilities { get }
  static var hasVideoSource: AVB17221ADPTalkerCapabilities { get }
}
@available(OSX 10.8, *)
struct AVB17221ADPListenerCapabilities : OptionSet {
  init(rawValue rawValue: UInt16)
  let rawValue: UInt16
  static var implemented: AVB17221ADPListenerCapabilities { get }
  static var hasOtherSink: AVB17221ADPListenerCapabilities { get }
  static var hasControlSink: AVB17221ADPListenerCapabilities { get }
  static var hasMediaClockSink: AVB17221ADPListenerCapabilities { get }
  static var hasSMPTESink: AVB17221ADPListenerCapabilities { get }
  static var hasMIDISink: AVB17221ADPListenerCapabilities { get }
  static var hasAudioSink: AVB17221ADPListenerCapabilities { get }
  static var hasVideoSink: AVB17221ADPListenerCapabilities { get }
}
@available(OSX 10.8, *)
struct AVB17221ADPControllerCapabilities : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var implemented: AVB17221ADPControllerCapabilities { get }
  static var hasLayer3Proxy: AVB17221ADPControllerCapabilities { get }
}
@available(OSX 10.8, *)
enum AVB17221AECPMessageType : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case aemCommand
  case aemResponse
  case addressAccessCommand
  case addressAccessResponse
  case legacyAVCCommand
  case legacyAVCResponse
  case vendorUniqueCommand
  case vendorUniqueResponse
}
@available(OSX 10.8, *)
enum AVB17221AECPStatusCode : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case success
  case notImplemented
  case noSuchDescriptor
  case entityLocked
  case entityAcquired
  case notAuthorized
  case insufficientPrivileges
  case badArguments
  case noResources
  case inProgress
  case entityMisbehaving
  case notSupported
  case streamIsRunning
  static var addressAccessAddressTooLow: AVB17221AECPStatusCode { get }
  static var addressAccessAddressTooHigh: AVB17221AECPStatusCode { get }
  static var addressAccessAddressInvalid: AVB17221AECPStatusCode { get }
  static var addressAccessTLVInvalid: AVB17221AECPStatusCode { get }
  static var addressAccessDataInvalid: AVB17221AECPStatusCode { get }
  static var addressAccessUnsupported: AVB17221AECPStatusCode { get }
  static var avcFailure: AVB17221AECPStatusCode { get }
}
@available(OSX 10.8, *)
enum AVB17221ACMPMessageType : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case connectTXCommand
  case connectTXResponse
  case disconnectTXCommand
  case disconnectTXResponse
  case getTXStateCommand
  case getTXStateResponse
  case connectRXCommand
  case connectRXResponse
  case disconnectRXCommand
  case disconnectRXResponse
  case getRXStateCommand
  case getRXStateResponse
  case getTXConnectionCommand
  case getTXConnectionResponse
}
@available(OSX 10.8, *)
enum AVB17221ACMPStatusCode : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case success
  case listenerUnknownID
  case talkerUnknownID
  case talkerDestMACFail
  case talkerNoStreamIndex
  case talkerNoBandwidth
  case talkerExclusive
  case listenerTalkerTimeout
  case listenerExclusive
  case stateUnavailable
  case notConnected
  case noSuchConnection
  case unableToSendMessage
  case talkerMisbehaving
  case listenerMisbehaving
  case srpFace
  case controllerNotAuthorized
  case incompatibleRequest
  case notSupported
}
@available(OSX 10.8, *)
struct AVB17221ACMPFlags : OptionSet {
  init(rawValue rawValue: UInt16)
  let rawValue: UInt16
  static var classB: AVB17221ACMPFlags { get }
  static var fastConnect: AVB17221ACMPFlags { get }
  static var savedState: AVB17221ACMPFlags { get }
  static var streamingWait: AVB17221ACMPFlags { get }
  static var supportsEncrypted: AVB17221ACMPFlags { get }
  static var encryptedPDU: AVB17221ACMPFlags { get }
  static var streamingTalkerFailed: AVB17221ACMPFlags { get }
}
@available(OSX 10.8, *)
enum AVB17221AEMCommandType : UInt16 {
  init?(rawValue rawValue: UInt16)
  var rawValue: UInt16 { get }
  case acquireEntity
  case lockEntity
  case entityAvailable
  case controllerAvailable
  case readDescriptor
  case writeDescriptor
  case setConfiguration
  case getConfiguration
  case setStreamFormat
  case getStreamFormat
  case setVideoFormat
  case getVideoFormat
  case setSensorFormat
  case getSensorFormat
  case setStreamInfo
  case getStreamInfo
  case setName
  case getName
  case setAssociationID
  case getAssociationID
  case setSamplingRate
  case getSamplingRate
  case setClockSource
  case getClockSource
  case setControl
  case getControl
  case incrementControl
  case decrementControl
  case setSignalSelector
  case getSignalSelector
  case setMixer
  case getMixer
  case setMatrix
  case getMatrix
  case startStreaming
  case stopStreaming
  case registerUnsolicitedNotification
  case deregisterUnsolicitedNotification
  case identifyNotification
  case getAVBInfo
  case getASPath
  case getCounters
  case reboot
  case getAudioMap
  case addAudioMapping
  case removeAudioMapping
  case getVideoMap
  case addVideoMapping
  case removeVideoMapping
  case getSensorMap
  case addSensorMapping
  case removeSensorMapping
  case startOperation
  case abortOperation
  case operationStatus
  case authenticateAddKey
  case authenticateDeleteKey
  case authenticateGetKeyList
  case authenticateGetKey
  case authenticateAddKeyToChain
  case authenticateDeleteKeyFromChain
  case authenticateGetKeychainList
  case authenticateGetIdentity
  case authenticateAddToken
  case authenticateDeleteToken
  case authenticate
  case deauthenticate
  case enableTransportSecurity
  case disableTransportSecurity
  case enableStreamEncryption
  case disableStreamEncryption
  case setMemoryObjectLength
  case getMemoryObjectLength
  case setStreamBackup
  case getStreamBackup
}
@available(OSX 10.9, *)
enum AVB17221AECPAddressAccessTLVMode : UInt8 {
  init?(rawValue rawValue: UInt8)
  var rawValue: UInt8 { get }
  case read
  case write
  case execute
}
let AVBErrorDomain: String
