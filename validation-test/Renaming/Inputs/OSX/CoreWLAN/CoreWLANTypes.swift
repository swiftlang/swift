
@available(OSX 10.6, *)
enum CWErr : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cwNoErr
  case cweapolErr
  case cwInvalidParameterErr
  case cwNoMemoryErr
  case cwUnknownErr
  case cwNotSupportedErr
  case cwInvalidFormatErr
  case cwTimeoutErr
  case cwUnspecifiedFailureErr
  case cwUnsupportedCapabilitiesErr
  case cwReassociationDeniedErr
  case cwAssociationDeniedErr
  case cwAuthenticationAlgorithmUnsupportedErr
  case cwInvalidAuthenticationSequenceNumberErr
  case cwChallengeFailureErr
  case cwapFullErr
  case cwUnsupportedRateSetErr
  case cwShortSlotUnsupportedErr
  case cwdsssofdmUnsupportedErr
  case cwInvalidInformationElementErr
  case cwInvalidGroupCipherErr
  case cwInvalidPairwiseCipherErr
  case cwInvalidAKMPErr
  case cwUnsupportedRSNVersionErr
  case cwInvalidRSNCapabilitiesErr
  case cwCipherSuiteRejectedErr
  case cwInvalidPMKErr
  case cwSupplicantTimeoutErr
  case cwhtFeaturesNotSupportedErr
  case cwpcoTransitionTimeNotSupportedErr
  case cwReferenceNotBoundErr
  case cwipcFailureErr
  case cwOperationNotPermittedErr
  case cwErr
}
@available(OSX 10.7, *)
enum CWPHYMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case modeNone
  case mode11a
  case mode11b
  case mode11g
  case mode11n
  case mode11ac
}
@available(OSX 10.7, *)
enum CWInterfaceMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case station
  case IBSS
  case hostAP
}
@available(OSX 10.7, *)
enum CWSecurity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case WEP
  case wpaPersonal
  case wpaPersonalMixed
  case wpa2Personal
  case personal
  case dynamicWEP
  case wpaEnterprise
  case wpaEnterpriseMixed
  case wpa2Enterprise
  case enterprise
  case unknown
}
@available(OSX 10.7, *)
enum CWIBSSModeSecurity : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case WEP40
  case WEP104
}
@available(OSX 10.7, *)
enum CWChannelWidth : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case widthUnknown
  case width20MHz
  case width40MHz
  case width80MHz
  case width160MHz
}
@available(OSX 10.7, *)
enum CWChannelBand : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case bandUnknown
  case band2GHz
  case band5GHz
}
@available(OSX 10.7, *)
struct CWCipherKeyFlags : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var unicast: CWCipherKeyFlags { get }
  static var multicast: CWCipherKeyFlags { get }
  static var tx: CWCipherKeyFlags { get }
  static var rx: CWCipherKeyFlags { get }
}
@available(OSX 10.10, *)
enum CWKeychainDomain : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case user
  case system
}
@available(OSX 10.10, *)
enum CWEventType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case powerDidChange
  case ssidDidChange
  case bssidDidChange
  case countryCodeDidChange
  case linkDidChange
  case linkQualityDidChange
  case modeDidChange
  case scanCacheUpdated
  case unknown
}
