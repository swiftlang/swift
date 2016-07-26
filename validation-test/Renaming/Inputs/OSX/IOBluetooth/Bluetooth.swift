
typealias BluetoothConnectionHandle = UInt16
typealias BluetoothLMPHandle = UInt8
var kBluetoothConnectionHandleNone: Int { get }
typealias BluetoothReasonCode = UInt8
typealias BluetoothEncryptionEnable = UInt8
var kBluetoothEncryptionEnableOff: Int { get }
var kBluetoothEncryptionEnableOn: Int { get }
var kBluetoothEncryptionEnableBREDRE0: Int { get }
var kBluetoothEncryptionEnableLEAESCCM: Int { get }
var kBluetoothEncryptionEnableBREDRAESCCM: Int { get }
typealias BluetoothKeyFlag = UInt8
var kBluetoothKeyFlagSemiPermanent: Int { get }
var kBluetoothKeyFlagTemporary: Int { get }
typealias BluetoothKeyType = UInt8
var kBluetoothKeyTypeCombination: Int { get }
var kBluetoothKeyTypeLocalUnit: Int { get }
var kBluetoothKeyTypeRemoteUnit: Int { get }
var kBluetoothKeyTypeDebugCombination: Int { get }
var kBluetoothKeyTypeUnauthenticatedCombination: Int { get }
var kBluetoothKeyTypeAuthenticatedCombination: Int { get }
var kBluetoothKeyTypeChangedCombination: Int { get }
typealias BluetoothPacketType = UInt16
var kBluetoothPacketTypeReserved1: Int { get }
var kBluetoothPacketType2DH1Omit: Int { get }
var kBluetoothPacketType3DH1Omit: Int { get }
var kBluetoothPacketTypeDM1: Int { get }
var kBluetoothPacketTypeDH1: Int { get }
var kBluetoothPacketTypeHV1: Int { get }
var kBluetoothPacketTypeHV2: Int { get }
var kBluetoothPacketTypeHV3: Int { get }
var kBluetoothPacketTypeDV: Int { get }
var kBluetoothPacketType2DH3Omit: Int { get }
var kBluetoothPacketType3DH3Omit: Int { get }
var kBluetoothPacketTypeAUX: Int { get }
var kBluetoothPacketTypeDM3: Int { get }
var kBluetoothPacketTypeDH3: Int { get }
var kBluetoothPacketType2DH5Omit: Int { get }
var kBluetoothPacketType3DM5Omit: Int { get }
var kBluetoothPacketTypeDM5: Int { get }
var kBluetoothPacketTypeDH5: Int { get }
var kBluetoothPacketTypeEnd: Int { get }
var kBluetoothSynchronousConnectionPacketTypeNone: Int { get }
var kBluetoothSynchronousConnectionPacketTypeHV1: Int { get }
var kBluetoothSynchronousConnectionPacketTypeHV2: Int { get }
var kBluetoothSynchronousConnectionPacketTypeHV3: Int { get }
var kBluetoothSynchronousConnectionPacketTypeEV3: Int { get }
var kBluetoothSynchronousConnectionPacketTypeEV4: Int { get }
var kBluetoothSynchronousConnectionPacketTypeEV5: Int { get }
var kBluetoothSynchronousConnectionPacketType2EV3Omit: Int { get }
var kBluetoothSynchronousConnectionPacketType3EV3Omit: Int { get }
var kBluetoothSynchronousConnectionPacketType2EV5Omit: Int { get }
var kBluetoothSynchronousConnectionPacketType3EV5Omit: Int { get }
var kBluetoothSynchronousConnectionPacketTypeFutureUse: Int { get }
var kBluetoothSynchronousConnectionPacketTypeAll: Int { get }
var kBluetoothSynchronousConnectionPacketTypeEnd: Int { get }
typealias BluetoothLAP = UInt32
var kBluetoothGeneralInquiryAccessCodeIndex: Int { get }
var kBluetoothGeneralInquiryAccessCodeLAPValue: Int { get }
var kBluetoothLimitedInquiryAccessCodeIndex: Int { get }
var kBluetoothLimitedInquiryAccessCodeLAPValue: Int { get }
var kBluetoothLimitedInquiryAccessCodeEnd: Int { get }
typealias BluetoothPageScanRepetitionMode = UInt8
var kBluetoothPageScanRepetitionModeR0: Int { get }
var kBluetoothPageScanRepetitionModeR1: Int { get }
var kBluetoothPageScanRepetitionModeR2: Int { get }
typealias BluetoothPageScanPeriodMode = UInt8
var kBluetoothPageScanPeriodModeP0: Int { get }
var kBluetoothPageScanPeriodModeP1: Int { get }
var kBluetoothPageScanPeriodModeP2: Int { get }
typealias BluetoothPageScanMode = UInt8
var kBluetoothPageScanModeMandatory: Int { get }
var kBluetoothPageScanModeOptional1: Int { get }
var kBluetoothPageScanModeOptional2: Int { get }
var kBluetoothPageScanModeOptional3: Int { get }
typealias BluetoothHCIPageScanType = UInt8
struct BluetoothHCIPageScanTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIPageScanTypeStandard: BluetoothHCIPageScanTypes { get }
var kBluetoothHCIPageScanTypeInterlaced: BluetoothHCIPageScanTypes { get }
var kBluetoothHCIPageScanTypeReservedStart: BluetoothHCIPageScanTypes { get }
var kBluetoothHCIPageScanTypeReservedEnd: BluetoothHCIPageScanTypes { get }
typealias BluetoothHCIErroneousDataReporting = UInt8
var kBluetoothHCIErroneousDataReportingDisabled: Int { get }
var kBluetoothHCIErroneousDataReportingEnabled: Int { get }
var kBluetoothHCIErroneousDataReportingReservedStart: Int { get }
var kBluetoothHCIErroneousDataReportingReservedEnd: Int { get }
struct BluetoothDeviceAddress {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothKey {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothIRK {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothPINCode {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothClassOfDevice = UInt32
typealias BluetoothServiceClassMajor = UInt32
typealias BluetoothDeviceClassMajor = UInt32
typealias BluetoothDeviceClassMinor = UInt32
var kBluetoothDeviceNameMaxLength: Int { get }
typealias BluetoothDeviceName = (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
typealias BluetoothClockOffset = UInt16
typealias BluetoothRole = UInt8
typealias BluetoothAllowRoleSwitch = UInt8
var kBluetoothDontAllowRoleSwitch: Int { get }
var kBluetoothAllowRoleSwitch: Int { get }
var kBluetoothRoleBecomeMaster: Int { get }
var kBluetoothRoleRemainSlave: Int { get }
struct BluetoothSetEventMask {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothPINType = UInt8
var kBluetoothL2CAPMaxPacketSize: Int { get }
var kBluetoothACLLogicalChannelReserved: Int { get }
var kBluetoothACLLogicalChannelL2CAPContinue: Int { get }
var kBluetoothACLLogicalChannelL2CAPStart: Int { get }
var kBluetoothACLLogicalChannelLMP: Int { get }
typealias BluetoothL2CAPChannelID = UInt16
var kBluetoothL2CAPChannelNull: Int { get }
var kBluetoothL2CAPChannelSignalling: Int { get }
var kBluetoothL2CAPChannelConnectionLessData: Int { get }
var kBluetoothL2CAPChannelAMPManagerProtocol: Int { get }
var kBluetoothL2CAPChannelAttributeProtocol: Int { get }
var kBluetoothL2CAPChannelLESignalling: Int { get }
var kBluetoothL2CAPChannelSecurityManager: Int { get }
var kBluetoothL2CAPChannelReservedStart: Int { get }
var kBluetoothL2CAPChannelLEAP: Int { get }
var kBluetoothL2CAPChannelLEAS: Int { get }
var kBluetoothL2CAPChannelMagnet: Int { get }
var kBluetoothL2CAPChannelReservedEnd: Int { get }
var kBluetoothL2CAPChannelDynamicStart: Int { get }
var kBluetoothL2CAPChannelDynamicEnd: Int { get }
var kBluetoothL2CAPChannelEnd: Int { get }
typealias BluetoothL2CAPGroupID = BluetoothL2CAPChannelID
typealias BluetoothL2CAPPSM = UInt16
struct BluetoothL2CAPCommandCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPCommandCodeReserved: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeCommandReject: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConnectionRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConnectionResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConfigureRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConfigureResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeDisconnectionRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeDisconnectionResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeEchoRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeEchoResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeInformationRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeInformationResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeCreateChannelRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeCreateChannelResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeMoveChannelRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeMoveChannelResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeMoveChannelConfirmation: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeMoveChannelConfirmationResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConnectionParameterUpdateRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeConnectionParameterUpdateResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeLECreditBasedConnectionRequest: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeLECreditBasedConnectionResponse: BluetoothL2CAPCommandCode { get }
var kBluetoothL2CAPCommandCodeLEFlowControlCredit: BluetoothL2CAPCommandCode { get }
struct BluetoothL2CAPCommandRejectReason : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPCommandRejectReasonCommandNotUnderstood: BluetoothL2CAPCommandRejectReason { get }
var kBluetoothL2CAPCommandRejectReasonSignallingMTUExceeded: BluetoothL2CAPCommandRejectReason { get }
var kBluetoothL2CAPCommandRejectReasonInvalidCIDInRequest: BluetoothL2CAPCommandRejectReason { get }
typealias BluetoothL2CAPMTU = UInt16
typealias BluetoothL2CAPLinkTimeout = UInt16
typealias BluetoothL2CAPFlushTimeout = UInt16
var kBluetoothL2CAPFlushTimeoutUseExisting: Int { get }
var kBluetoothL2CAPFlushTimeoutImmediate: Int { get }
var kBluetoothL2CAPFlushTimeoutForever: Int { get }
var kBluetoothL2CAPFlushTimeoutEnd: Int { get }
struct BluetoothL2CAPQualityOfServiceOptions {
  var flags: UInt8
  var serviceType: UInt8
  var tokenRate: UInt32
  var tokenBucketSize: UInt32
  var peakBandwidth: UInt32
  var latency: UInt32
  var delayVariation: UInt32
  init()
  init(flags flags: UInt8, serviceType serviceType: UInt8, tokenRate tokenRate: UInt32, tokenBucketSize tokenBucketSize: UInt32, peakBandwidth peakBandwidth: UInt32, latency latency: UInt32, delayVariation delayVariation: UInt32)
}
struct BluetoothL2CAPRetransmissionAndFlowControlOptions {
  var flags: UInt8
  var txWindowSize: UInt8
  var maxTransmit: UInt8
  var retransmissionTimeout: UInt16
  var monitorTimeout: UInt16
  var maxPDUPayloadSize: UInt16
  init()
  init(flags flags: UInt8, txWindowSize txWindowSize: UInt8, maxTransmit maxTransmit: UInt8, retransmissionTimeout retransmissionTimeout: UInt16, monitorTimeout monitorTimeout: UInt16, maxPDUPayloadSize maxPDUPayloadSize: UInt16)
}
var kBluetoothL2CAPInfoTypeMaxConnectionlessMTUSize: Int { get }
var kBluetoothL2CAPPacketHeaderSize: Int { get }
typealias BluetoothL2CAPByteCount = UInt16
typealias BluetoothL2CAPCommandID = UInt8
typealias BluetoothL2CAPCommandByteCount = UInt16
struct BluetoothL2CAPConnectionResult : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPConnectionResultSuccessful: BluetoothL2CAPConnectionResult { get }
var kBluetoothL2CAPConnectionResultPending: BluetoothL2CAPConnectionResult { get }
var kBluetoothL2CAPConnectionResultRefusedPSMNotSupported: BluetoothL2CAPConnectionResult { get }
var kBluetoothL2CAPConnectionResultRefusedSecurityBlock: BluetoothL2CAPConnectionResult { get }
var kBluetoothL2CAPConnectionResultRefusedNoResources: BluetoothL2CAPConnectionResult { get }
struct BluetoothL2CAPConnectionStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPConnectionStatusNoInfoAvailable: BluetoothL2CAPConnectionStatus { get }
var kBluetoothL2CAPConnectionStatusAuthenticationPending: BluetoothL2CAPConnectionStatus { get }
var kBluetoothL2CAPConnectionStatusAuthorizationPending: BluetoothL2CAPConnectionStatus { get }
struct BluetoothL2CAPConfigurationResult : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPConfigurationResultSuccess: BluetoothL2CAPConfigurationResult { get }
var kBluetoothL2CAPConfigurationResultUnacceptableParams: BluetoothL2CAPConfigurationResult { get }
var kBluetoothL2CAPConfigurationResultRejected: BluetoothL2CAPConfigurationResult { get }
var kBluetoothL2CAPConfigurationResultUnknownOptions: BluetoothL2CAPConfigurationResult { get }
struct BluetoothL2CAPConfigurationOption : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPConfigurationOptionMTU: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionFlushTimeout: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionQoS: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionRetransmissionAndFlowControl: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionFrameCheckSequence: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionExtendedFlowSpecification: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionExtendedWindowSize: BluetoothL2CAPConfigurationOption { get }
var kBluetoothL2CAPConfigurationOptionMTULength: Int { get }
var kBluetoothL2CAPConfigurationOptionFlushTimeoutLength: Int { get }
var kBluetoothL2CAPConfigurationOptionQoSLength: Int { get }
var kBluetoothL2CAPConfigurationOptionRetransmissionAndFlowControlLength: Int { get }
struct BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPConfigurationBasicL2CAPModeFlag: BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags { get }
var kBluetoothL2CAPConfigurationRetransmissionModeFlag: BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags { get }
var kBluetoothL2CAPConfigurationFlowControlModeFlag: BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags { get }
var kBluetoothL2CAPConfigurationEnhancedRetransmissionMode: BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags { get }
var kBluetoothL2CAPConfigurationStreamingMode: BluetoothL2CAPConfigurationRetransmissionAndFlowControlFlags { get }
struct BluetoothL2CAPInformationType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPInformationTypeConnectionlessMTU: BluetoothL2CAPInformationType { get }
var kBluetoothL2CAPInformationTypeExtendedFeatures: BluetoothL2CAPInformationType { get }
var kBluetoothL2CAPInformationTypeFixedChannelsSupported: BluetoothL2CAPInformationType { get }
struct BluetoothL2CAPInformationResult : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPInformationResultSuccess: BluetoothL2CAPInformationResult { get }
var kBluetoothL2CAPInformationResultNotSupported: BluetoothL2CAPInformationResult { get }
struct BluetoothL2CAPInformationExtendedFeaturesMask : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPInformationNoExtendedFeatures: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationFlowControlMode: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationRetransmissionMode: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationBidirectionalQoS: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationEnhancedRetransmissionMode: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationStreamingMode: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationFCSOption: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationExtendedFlowSpecification: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationFixedChannels: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPInformationExtendedWindowSize: BluetoothL2CAPInformationExtendedFeaturesMask { get }
var kBluetoothL2CAPUnicastConnectionlessDataReception: BluetoothL2CAPInformationExtendedFeaturesMask { get }
struct BluetoothL2CAPQoSType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothL2CAPQoSTypeNoTraffic: BluetoothL2CAPQoSType { get }
var kBluetoothL2CAPQoSTypeBestEffort: BluetoothL2CAPQoSType { get }
var kBluetoothL2CAPQoSTypeGuaranteed: BluetoothL2CAPQoSType { get }
var kBluetoothL2CAPMTULowEnergyDefault: UInt32 { get }
var kBluetoothL2CAPMTUMinimum: UInt32 { get }
var kBluetoothL2CAPMTUDefault: UInt32 { get }
var kBluetoothL2CAPMTUMaximum: UInt32 { get }
var kBluetoothL2CAPMTUStart: UInt32 { get }
var kBluetoothL2CAPMTUSIG: UInt32 { get }
var kBluetoothL2CAPFlushTimeoutDefault: UInt32 { get }
var kBluetoothL2CAPQoSFlagsDefault: UInt32 { get }
var kBluetoothL2CAPQoSTypeDefault: UInt32 { get }
var kBluetoothL2CAPQoSTokenRateDefault: UInt32 { get }
var kBluetoothL2CAPQoSTokenBucketSizeDefault: UInt32 { get }
var kBluetoothL2CAPQoSPeakBandwidthDefault: UInt32 { get }
var kBluetoothL2CAPQoSLatencyDefault: UInt32 { get }
var kBluetoothL2CAPQoSDelayVariationDefault: UInt32 { get }
var kBluetoothLESMPTimeout: Int32 { get }
var kBluetoothLESMPMinEncryptionKeySize: Int32 { get }
var kBluetoothLESMPMaxEncryptionKeySize: Int32 { get }
struct BluetoothLESecurityManagerKeyDistributionFormat : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerEncryptionKey: BluetoothLESecurityManagerKeyDistributionFormat { get }
var kBluetoothLESecurityManagerIDKey: BluetoothLESecurityManagerKeyDistributionFormat { get }
var kBluetoothLESecurityManagerSignKey: BluetoothLESecurityManagerKeyDistributionFormat { get }
var kBluetoothLESecurityManagerLinkKey: BluetoothLESecurityManagerKeyDistributionFormat { get }
struct BluetoothLESecurityManagerCommandCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerCommandCodeReserved: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingRequest: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingResponse: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingConfirm: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingRandom: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingFailed: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeEncryptionInfo: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeMasterIdentification: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeIdentityInfo: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeIdentityAddressInfo: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeSigningInfo: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeSecurityRequest: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingPublicKey: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingDHKeyCheck: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodePairingKeypressNotification: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeReservedStart: BluetoothLESecurityManagerCommandCode { get }
var kBluetoothLESecurityManagerCommandCodeReservedEnd: BluetoothLESecurityManagerCommandCode { get }
struct BluetoothLESecurityManagerUserInputCapability : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerUserInputCapabilityNoInput: BluetoothLESecurityManagerUserInputCapability { get }
var kBluetoothLESecurityManagerUserInputCapabilityYesNo: BluetoothLESecurityManagerUserInputCapability { get }
var kBluetoothLESecurityManagerUserInputCapabilityKeyboard: BluetoothLESecurityManagerUserInputCapability { get }
struct BluetoothLESecurityManagerUserOutputCapability : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerUserOutputCapabilityNoOutput: BluetoothLESecurityManagerUserOutputCapability { get }
var kBluetoothLESecurityManagerUserOutputCapabilityNumericOutput: BluetoothLESecurityManagerUserOutputCapability { get }
struct BluetoothLESecurityManagerIOCapability : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerIOCapabilityDisplayOnly: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityDisplayYesNo: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityKeyboardOnly: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityNoInputNoOutput: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityKeyboardDisplay: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityReservedStart: BluetoothLESecurityManagerIOCapability { get }
var kBluetoothLESecurityManagerIOCapabilityReservedEnd: BluetoothLESecurityManagerIOCapability { get }
struct BluetoothLESecurityManagerOOBData : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerOOBAuthenticationDataNotPresent: BluetoothLESecurityManagerOOBData { get }
var kBluetoothLESecurityManagerOOBAuthenticationDataPresent: BluetoothLESecurityManagerOOBData { get }
var kBluetoothLESecurityManagerOOBDataReservedStart: BluetoothLESecurityManagerOOBData { get }
var kBluetoothLESecurityManagerOOBDataReservedEnd: BluetoothLESecurityManagerOOBData { get }
var kBluetoothLESecurityManagerNoBonding: Int { get }
var kBluetoothLESecurityManagerBonding: Int { get }
var kBluetoothLESecurityManagerReservedStart: Int { get }
var kBluetoothLESecurityManagerReservedEnd: Int { get }
struct BluetoothLESecurityManagerPairingFailedReasonCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerReasonCodeReserved: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodePasskeyEntryFailed: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeOOBNotAvailbale: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeAuthenticationRequirements: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeConfirmValueFailed: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodePairingNotSupported: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeEncryptionKeySize: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeCommandNotSupported: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeUnspecifiedReason: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeRepeatedAttempts: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeInvalidParameters: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeDHKeyCheckFailed: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeNumericComparisonFailed: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeBREDRPairingInProgress: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeCrossTransportKeyDerivationGenerationNotAllowed: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeReservedStart: BluetoothLESecurityManagerPairingFailedReasonCode { get }
var kBluetoothLESecurityManagerReasonCodeReservedEnd: BluetoothLESecurityManagerPairingFailedReasonCode { get }
struct BluetoothLESecurityManagerKeypressNotificationType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothLESecurityManagerNotificationTypePasskeyEntryStarted: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypePasskeyDigitEntered: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypePasskeyDigitErased: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypePasskeyCleared: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypePasskeyEntryCompleted: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypeReservedStart: BluetoothLESecurityManagerKeypressNotificationType { get }
var kBluetoothLESecurityManagerNotificationTypeReservedEnd: BluetoothLESecurityManagerKeypressNotificationType { get }
struct BluetoothAMPManagerCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerCodeReserved: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPCommandReject: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPDiscoverRequest: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPDiscoverResponse: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPChangeNotify: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPChangeResponse: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPGetInfoRequest: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPGetInfoResponse: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPGetAssocRequest: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPGetAssocResponse: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPCreatePhysicalLinkRequest: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPCreatePhysicalLinkResponse: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPDisconnectPhysicalLinkRequest: BluetoothAMPManagerCode { get }
var kBluetoothAMPManagerCodeAMPDisconnectPhysicalLinkResponse: BluetoothAMPManagerCode { get }
struct BluetoothAMPCommandRejectReason : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerCommandRejectReasonCommandNotRecognized: BluetoothAMPCommandRejectReason { get }
struct BluetoothAMPDiscoverResponseControllerStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerDiscoverResponseControllerStatusPoweredDown: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusBluetoothOnly: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusNoCapacity: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusLowCapacity: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusMediumCapacity: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusHighCapacity: BluetoothAMPDiscoverResponseControllerStatus { get }
var kBluetoothAMPManagerDiscoverResponseControllerStatusFullCapacity: BluetoothAMPDiscoverResponseControllerStatus { get }
struct BluetoothAMPGetInfoResponseStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerGetInfoResponseSuccess: BluetoothAMPGetInfoResponseStatus { get }
var kBluetoothAMPManagerGetInfoResponseInvalidControllerID: BluetoothAMPGetInfoResponseStatus { get }
struct BluetoothAMPGetAssocResponseStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerGetAssocResponseSuccess: BluetoothAMPGetAssocResponseStatus { get }
var kBluetoothAMPManagerGetAssocResponseInvalidControllerID: BluetoothAMPGetAssocResponseStatus { get }
struct BluetoothAMPCreatePhysicalLinkResponseStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerCreatePhysicalLinkResponseSuccess: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponseInvalidControllerID: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponseUnableToStartLinkCreation: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponseCollisionOccurred: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponseAMPDisconnectedPhysicalLinkRequestReceived: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponsePhysicalLinkAlreadyExists: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerCreatePhysicalLinkResponseSecurityViolation: BluetoothAMPCreatePhysicalLinkResponseStatus { get }
struct BluetoothAMPDisconnectPhysicalLinkResponseStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAMPManagerDisconnectPhysicalLinkResponseSuccess: BluetoothAMPDisconnectPhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerDisconnectPhysicalLinkResponseInvalidControllerID: BluetoothAMPDisconnectPhysicalLinkResponseStatus { get }
var kBluetoothAMPManagerDisconnectPhysicalLinkResponseNoPhysicalLink: BluetoothAMPDisconnectPhysicalLinkResponseStatus { get }
typealias BluetoothHCICommandOpCodeGroup = UInt8
typealias BluetoothHCICommandOpCodeCommand = UInt16
typealias BluetoothHCICommandOpCode = UInt16
typealias BluetoothHCIVendorCommandSelector = UInt32
var kBluetoothHCIOpCodeNoOp: Int { get }
var kBluetoothHCICommandGroupNoOp: Int { get }
var kBluetoothHCICommandNoOp: Int { get }
var kBluetoothHCICommandGroupLinkControl: Int { get }
var kBluetoothHCICommandInquiry: Int { get }
var kBluetoothHCICommandInquiryCancel: Int { get }
var kBluetoothHCICommandPeriodicInquiryMode: Int { get }
var kBluetoothHCICommandExitPeriodicInquiryMode: Int { get }
var kBluetoothHCICommandCreateConnection: Int { get }
var kBluetoothHCICommandDisconnect: Int { get }
var kBluetoothHCICommandAddSCOConnection: Int { get }
var kBluetoothHCICommandCreateConnectionCancel: Int { get }
var kBluetoothHCICommandAcceptConnectionRequest: Int { get }
var kBluetoothHCICommandRejectConnectionRequest: Int { get }
var kBluetoothHCICommandLinkKeyRequestReply: Int { get }
var kBluetoothHCICommandLinkKeyRequestNegativeReply: Int { get }
var kBluetoothHCICommandPINCodeRequestReply: Int { get }
var kBluetoothHCICommandPINCodeRequestNegativeReply: Int { get }
var kBluetoothHCICommandChangeConnectionPacketType: Int { get }
var kBluetoothHCICommandAuthenticationRequested: Int { get }
var kBluetoothHCICommandSetConnectionEncryption: Int { get }
var kBluetoothHCICommandChangeConnectionLinkKey: Int { get }
var kBluetoothHCICommandMasterLinkKey: Int { get }
var kBluetoothHCICommandRemoteNameRequest: Int { get }
var kBluetoothHCICommandReadRemoteSupportedFeatures: Int { get }
var kBluetoothHCICommandReadRemoteExtendedFeatures: Int { get }
var kBluetoothHCICommandReadRemoteVersionInformation: Int { get }
var kBluetoothHCICommandReadClockOffset: Int { get }
var kBluetoothHCICommandRemoteNameRequestCancel: Int { get }
var kBluetoothHCICommandReadLMPHandle: Int { get }
var kBluetoothHCICommandSetupSynchronousConnection: Int { get }
var kBluetoothHCICommandAcceptSynchronousConnectionRequest: Int { get }
var kBluetoothHCICommandRejectSynchronousConnectionRequest: Int { get }
var kBluetoothHCICommandIOCapabilityRequestReply: Int { get }
var kBluetoothHCICommandUserConfirmationRequestReply: Int { get }
var kBluetoothHCICommandUserConfirmationRequestNegativeReply: Int { get }
var kBluetoothHCICommandUserPasskeyRequestReply: Int { get }
var kBluetoothHCICommandUserPasskeyRequestNegativeReply: Int { get }
var kBluetoothHCICommandRemoteOOBDataRequestReply: Int { get }
var kBluetoothHCICommandRemoteOOBDataRequestNegativeReply: Int { get }
var kBluetoothHCICommandIOCapabilityRequestNegativeReply: Int { get }
var kBluetoothHCICommandEnhancedSetupSynchronousConnection: Int { get }
var kBluetoothHCICommandEnhancedAcceptSynchronousConnectionRequest: Int { get }
var kBluetoothHCICommandTruncatedPage: Int { get }
var kBluetoothHCICommandTruncatedPageCancel: Int { get }
var kBluetoothHCICommandSetConnectionlessSlaveBroadcast: Int { get }
var kBluetoothHCICommandSetConnectionlessSlaveBroadcastReceive: Int { get }
var kBluetoothHCICommandStartSynchronizationTrain: Int { get }
var kBluetoothHCICommandReceiveSynchronizationTrain: Int { get }
var kBluetoothHCICommandRemoteOOBExtendedDataRequestReply: Int { get }
var kBluetoothHCICommandGroupLinkPolicy: Int { get }
var kBluetoothHCICommandHoldMode: Int { get }
var kBluetoothHCICommandSniffMode: Int { get }
var kBluetoothHCICommandExitSniffMode: Int { get }
var kBluetoothHCICommandParkMode: Int { get }
var kBluetoothHCICommandExitParkMode: Int { get }
var kBluetoothHCICommandQoSSetup: Int { get }
var kBluetoothHCICommandRoleDiscovery: Int { get }
var kBluetoothHCICommandSwitchRole: Int { get }
var kBluetoothHCICommandReadLinkPolicySettings: Int { get }
var kBluetoothHCICommandWriteLinkPolicySettings: Int { get }
var kBluetoothHCICommandReadDefaultLinkPolicySettings: Int { get }
var kBluetoothHCICommandWriteDefaultLinkPolicySettings: Int { get }
var kBluetoothHCICommandFlowSpecification: Int { get }
var kBluetoothHCICommandSniffSubrating: Int { get }
var kBluetoothHCICommandAcceptSniffRequest: Int { get }
var kBluetoothHCICommandRejectSniffRequest: Int { get }
var kBluetoothHCICommandGroupHostController: Int { get }
var kBluetoothHCICommandSetEventMask: Int { get }
var kBluetoothHCICommandReset: Int { get }
var kBluetoothHCICommandSetEventFilter: Int { get }
var kBluetoothHCICommandFlush: Int { get }
var kBluetoothHCICommandReadPINType: Int { get }
var kBluetoothHCICommandWritePINType: Int { get }
var kBluetoothHCICommandCreateNewUnitKey: Int { get }
var kBluetoothHCICommandReadStoredLinkKey: Int { get }
var kBluetoothHCICommandWriteStoredLinkKey: Int { get }
var kBluetoothHCICommandDeleteStoredLinkKey: Int { get }
var kBluetoothHCICommandChangeLocalName: Int { get }
var kBluetoothHCICommandReadLocalName: Int { get }
var kBluetoothHCICommandReadConnectionAcceptTimeout: Int { get }
var kBluetoothHCICommandWriteConnectionAcceptTimeout: Int { get }
var kBluetoothHCICommandReadPageTimeout: Int { get }
var kBluetoothHCICommandWritePageTimeout: Int { get }
var kBluetoothHCICommandReadScanEnable: Int { get }
var kBluetoothHCICommandWriteScanEnable: Int { get }
var kBluetoothHCICommandReadPageScanActivity: Int { get }
var kBluetoothHCICommandWritePageScanActivity: Int { get }
var kBluetoothHCICommandReadInquiryScanActivity: Int { get }
var kBluetoothHCICommandWriteInquiryScanActivity: Int { get }
var kBluetoothHCICommandReadAuthenticationEnable: Int { get }
var kBluetoothHCICommandWriteAuthenticationEnable: Int { get }
var kBluetoothHCICommandReadEncryptionMode: Int { get }
var kBluetoothHCICommandWriteEncryptionMode: Int { get }
var kBluetoothHCICommandReadClassOfDevice: Int { get }
var kBluetoothHCICommandWriteClassOfDevice: Int { get }
var kBluetoothHCICommandReadVoiceSetting: Int { get }
var kBluetoothHCICommandWriteVoiceSetting: Int { get }
var kBluetoothHCICommandReadAutomaticFlushTimeout: Int { get }
var kBluetoothHCICommandWriteAutomaticFlushTimeout: Int { get }
var kBluetoothHCICommandReadNumberOfBroadcastRetransmissions: Int { get }
var kBluetoothHCICommandWriteNumberOfBroadcastRetransmissions: Int { get }
var kBluetoothHCICommandReadHoldModeActivity: Int { get }
var kBluetoothHCICommandWriteHoldModeActivity: Int { get }
var kBluetoothHCICommandReadTransmitPowerLevel: Int { get }
var kBluetoothHCICommandReadSCOFlowControlEnable: Int { get }
var kBluetoothHCICommandWriteSCOFlowControlEnable: Int { get }
var kBluetoothHCICommandSetHostControllerToHostFlowControl: Int { get }
var kBluetoothHCICommandHostBufferSize: Int { get }
var kBluetoothHCICommandHostNumberOfCompletedPackets: Int { get }
var kBluetoothHCICommandReadLinkSupervisionTimeout: Int { get }
var kBluetoothHCICommandWriteLinkSupervisionTimeout: Int { get }
var kBluetoothHCICommandReadNumberOfSupportedIAC: Int { get }
var kBluetoothHCICommandReadCurrentIACLAP: Int { get }
var kBluetoothHCICommandWriteCurrentIACLAP: Int { get }
var kBluetoothHCICommandReadPageScanPeriodMode: Int { get }
var kBluetoothHCICommandWritePageScanPeriodMode: Int { get }
var kBluetoothHCICommandReadPageScanMode: Int { get }
var kBluetoothHCICommandWritePageScanMode: Int { get }
var kBluetoothHCICommandSetAFHClassification: Int { get }
var kBluetoothHCICommandReadInquiryScanType: Int { get }
var kBluetoothHCICommandWriteInquiryScanType: Int { get }
var kBluetoothHCICommandReadInquiryMode: Int { get }
var kBluetoothHCICommandWriteInquiryMode: Int { get }
var kBluetoothHCICommandReadPageScanType: Int { get }
var kBluetoothHCICommandWritePageScanType: Int { get }
var kBluetoothHCICommandReadAFHChannelAssessmentMode: Int { get }
var kBluetoothHCICommandWriteAFHChannelAssessmentMode: Int { get }
var kBluetoothHCICommandReadExtendedInquiryResponse: Int { get }
var kBluetoothHCICommandWriteExtendedInquiryResponse: Int { get }
var kBluetoothHCICommandRefreshEncryptionKey: Int { get }
var kBluetoothHCICommandReadSimplePairingMode: Int { get }
var kBluetoothHCICommandWriteSimplePairingMode: Int { get }
var kBluetoothHCICommandReadLocalOOBData: Int { get }
var kBluetoothHCICommandReadInquiryResponseTransmitPower: Int { get }
var kBluetoothHCICommandWriteInquiryResponseTransmitPower: Int { get }
var kBluetoothHCICommandSendKeypressNotification: Int { get }
var kBluetoothHCICommandReadDefaultErroneousDataReporting: Int { get }
var kBluetoothHCICommandWriteDefaultErroneousDataReporting: Int { get }
var kBluetoothHCICommandEnhancedFlush: Int { get }
var kBluetoothHCICommandReadLogicalLinkAcceptTimeout: Int { get }
var kBluetoothHCICommandWriteLogicalLinkAcceptTimeout: Int { get }
var kBluetoothHCICommandSetEventMaskPageTwo: Int { get }
var kBluetoothHCICommandReadLocationData: Int { get }
var kBluetoothHCICommandWriteLocationData: Int { get }
var kBluetoothHCICommandReadFlowControlMode: Int { get }
var kBluetoothHCICommandWriteFlowControlMode: Int { get }
var kBluetoothHCICommandReadEnhancedTransmitPowerLevel: Int { get }
var kBluetoothHCICommandReadBestEffortFlushTimeout: Int { get }
var kBluetoothHCICommandWriteBestEffortFlushTimeout: Int { get }
var kBluetoothHCICommandShortRangeMode: Int { get }
var kBluetoothHCICommandReadLEHostSupported: Int { get }
var kBluetoothHCICommandWriteLEHostSupported: Int { get }
var kBluetoothHCICommandSetMWSChannelParameters: Int { get }
var kBluetoothHCICommandSetExternalFrameConfiguration: Int { get }
var kBluetoothHCICommandSetMWSSignaling: Int { get }
var kBluetoothHCICommandSetMWSTransportLayer: Int { get }
var kBluetoothHCICommandSetMWSScanFrequencyTable: Int { get }
var kBluetoothHCICommandSetMWSPATTERNConfiguration: Int { get }
var kBluetoothHCICommandSetReservedLTADDR: Int { get }
var kBluetoothHCICommandDeleteReservedLTADDR: Int { get }
var kBluetoothHCICommandSetConnectionlessSlaveBroadcastData: Int { get }
var kBluetoothHCICommandReadSynchronizationTrainParameters: Int { get }
var kBluetoothHCICommandWriteSynchronizationTrainParameters: Int { get }
var kBluetoothHCICommandReadSecureConnectionsHostSupport: Int { get }
var kBluetoothHCICommandWriteSecureConnectionsHostSupport: Int { get }
var kBluetoothHCICommandReadAuthenticatedPayloadTimeout: Int { get }
var kBluetoothHCICommandWriteAuthenticatedPayloadTimeout: Int { get }
var kBluetoothHCICommandReadLocalOOBExtendedData: Int { get }
var kBluetoothHCICommandReadExtendedPageTimeout: Int { get }
var kBluetoothHCICommandWriteExtendedPageTimeout: Int { get }
var kBluetoothHCICommandReadExtendedInquiryLength: Int { get }
var kBluetoothHCICommandWriteExtendedInquiryLength: Int { get }
var kBluetoothHCICommandGroupInformational: Int { get }
var kBluetoothHCICommandReadLocalVersionInformation: Int { get }
var kBluetoothHCICommandReadLocalSupportedCommands: Int { get }
var kBluetoothHCICommandReadLocalSupportedFeatures: Int { get }
var kBluetoothHCICommandReadLocalExtendedFeatures: Int { get }
var kBluetoothHCICommandReadBufferSize: Int { get }
var kBluetoothHCICommandReadCountryCode: Int { get }
var kBluetoothHCICommandReadDeviceAddress: Int { get }
var kBluetoothHCICommandReadDataBlockSize: Int { get }
var kBluetoothHCICommandReadLocalSupportedCodecs: Int { get }
var kBluetoothHCICommandGroupStatus: Int { get }
var kBluetoothHCICommandReadFailedContactCounter: Int { get }
var kBluetoothHCICommandResetFailedContactCounter: Int { get }
var kBluetoothHCICommandGetLinkQuality: Int { get }
var kBluetoothHCICommandReadRSSI: Int { get }
var kBluetoothHCICommandReadAFHMappings: Int { get }
var kBluetoothHCICommandReadClock: Int { get }
var kBluetoothHCICommandReadEncryptionKeySize: Int { get }
var kBluetoothHCICommandReadLocalAMPInfo: Int { get }
var kBluetoothHCICommandReadLocalAMPASSOC: Int { get }
var kBluetoothHCICommandWriteRemoteAMPASSOC: Int { get }
var kBluetoothHCICommandGetMWSTransportLayerConfiguration: Int { get }
var kBluetoothHCICommandSetTriggeredClockCapture: Int { get }
var kBluetoothHCICommandGroupTesting: Int { get }
var kBluetoothHCICommandReadLoopbackMode: Int { get }
var kBluetoothHCICommandWriteLoopbackMode: Int { get }
var kBluetoothHCICommandEnableDeviceUnderTestMode: Int { get }
var kBluetoothHCICommandWriteSimplePairingDebugMode: Int { get }
var kBluetoothHCICommandEnableAMPReceiverReports: Int { get }
var kBluetoothHCICommandAMPTestEnd: Int { get }
var kBluetoothHCICommandAMPTest: Int { get }
var kBluetoothHCICommandGroupLowEnergy: Int { get }
var kBluetoothHCICommandLESetEventMask: Int { get }
var kBluetoothHCICommandLEReadBufferSize: Int { get }
var kBluetoothHCICommandLEReadLocalSupportedFeatures: Int { get }
var kBluetoothHCICommandLESetRandomAddress: Int { get }
var kBluetoothHCICommandLESetAdvertisingParameters: Int { get }
var kBluetoothHCICommandLEReadAdvertisingChannelTxPower: Int { get }
var kBluetoothHCICommandLESetAdvertisingData: Int { get }
var kBluetoothHCICommandLESetScanResponseData: Int { get }
var kBluetoothHCICommandLESetAdvertiseEnable: Int { get }
var kBluetoothHCICommandLESetScanParameters: Int { get }
var kBluetoothHCICommandLESetScanEnable: Int { get }
var kBluetoothHCICommandLECreateConnection: Int { get }
var kBluetoothHCICommandLECreateConnectionCancel: Int { get }
var kBluetoothHCICommandLEReadWhiteListSize: Int { get }
var kBluetoothHCICommandLEClearWhiteList: Int { get }
var kBluetoothHCICommandLEAddDeviceToWhiteList: Int { get }
var kBluetoothHCICommandLERemoveDeviceFromWhiteList: Int { get }
var kBluetoothHCICommandLEConnectionUpdate: Int { get }
var kBluetoothHCICommandLESetHostChannelClassification: Int { get }
var kBluetoothHCICommandLEReadChannelMap: Int { get }
var kBluetoothHCICommandLEReadRemoteUsedFeatures: Int { get }
var kBluetoothHCICommandLEEncrypt: Int { get }
var kBluetoothHCICommandLERand: Int { get }
var kBluetoothHCICommandLEStartEncryption: Int { get }
var kBluetoothHCICommandLELongTermKeyRequestReply: Int { get }
var kBluetoothHCICommandLELongTermKeyRequestNegativeReply: Int { get }
var kBluetoothHCICommandLEReadSupportedStates: Int { get }
var kBluetoothHCICommandLEReceiverTest: Int { get }
var kBluetoothHCICommandLETransmitterTest: Int { get }
var kBluetoothHCICommandLETestEnd: Int { get }
var kBluetoothHCICommandLERemoteConnectionParameterRequestReply: Int { get }
var kBluetoothHCICommandLERemoteConnectionParameterRequestNegativeReply: Int { get }
var kBluetoothHCICommandLESetDataLength: Int { get }
var kBluetoothHCICommandLEReadSuggestedDefaultDataLength: Int { get }
var kBluetoothHCICommandLEWriteSuggestedDefaultDataLength: Int { get }
var kBluetoothHCICommandLEReadLocalP256PublicKey: Int { get }
var kBluetoothHCICommandLEGenerateDHKey: Int { get }
var kBluetoothHCICommandLEAddDeviceToResolvingList: Int { get }
var kBluetoothHCICommandLERemoveDeviceFromResolvingList: Int { get }
var kBluetoothHCICommandLEClearResolvingList: Int { get }
var kBluetoothHCICommandLEReadResolvingListSize: Int { get }
var kBluetoothHCICommandLEReadPeerResolvableAddress: Int { get }
var kBluetoothHCICommandLEReadLocalResolvableAddress: Int { get }
var kBluetoothHCICommandLESetAddressResolutionEnable: Int { get }
var kBluetoothHCICommandLESetResolvablePrivateAddressTimeout: Int { get }
var kBluetoothHCICommandLEReadMaximumDataLength: Int { get }
var kBluetoothHCICommandGroupLogoTesting: Int { get }
var kBluetoothHCICommandGroupVendorSpecific: Int { get }
var kBluetoothHCICommandGroupMax: Int { get }
var kBluetoothHCICommandMax: Int { get }
typealias BluetoothHCIQoSFlags = UInt8
typealias BluetoothHCIParamByteCount = UInt8
typealias BluetoothHCIACLDataByteCount = UInt16
typealias BluetoothHCISCODataByteCount = UInt8
typealias BluetoothHCIInquiryLength = UInt8
typealias BluetoothHCIResponseCount = UInt8
typealias BluetoothHCICountryCode = UInt8
typealias BluetoothHCIModeInterval = UInt16
typealias BluetoothHCISniffAttemptCount = UInt16
typealias BluetoothHCISniffTimeout = UInt16
typealias BluetoothHCIParkModeBeaconInterval = UInt16
typealias BluetoothMaxSlots = UInt8
typealias BluetoothManufacturerName = UInt16
typealias BluetoothLMPVersion = UInt8
typealias BluetoothLMPSubversion = UInt16
typealias BluetoothHCIConnectionMode = UInt8
struct BluetoothHCIConnectionModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kConnectionActiveMode: BluetoothHCIConnectionModes { get }
var kConnectionHoldMode: BluetoothHCIConnectionModes { get }
var kConnectionSniffMode: BluetoothHCIConnectionModes { get }
var kConnectionParkMode: BluetoothHCIConnectionModes { get }
var kConnectionModeReservedForFutureUse: BluetoothHCIConnectionModes { get }
struct BluetoothHCISupportedCommands {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCISupportedFeatures {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothHCIPageNumber = UInt8
struct BluetoothHCIExtendedFeaturesInfo {
  var page: BluetoothHCIPageNumber
  var maxPage: BluetoothHCIPageNumber
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(page page: BluetoothHCIPageNumber, maxPage maxPage: BluetoothHCIPageNumber, data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothFeatureBits : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothFeatureThreeSlotPackets: BluetoothFeatureBits { get }
var kBluetoothFeatureFiveSlotPackets: BluetoothFeatureBits { get }
var kBluetoothFeatureEncryption: BluetoothFeatureBits { get }
var kBluetoothFeatureSlotOffset: BluetoothFeatureBits { get }
var kBluetoothFeatureTimingAccuracy: BluetoothFeatureBits { get }
var kBluetoothFeatureSwitchRoles: BluetoothFeatureBits { get }
var kBluetoothFeatureHoldMode: BluetoothFeatureBits { get }
var kBluetoothFeatureSniffMode: BluetoothFeatureBits { get }
var kBluetoothFeatureParkMode: BluetoothFeatureBits { get }
var kBluetoothFeatureRSSI: BluetoothFeatureBits { get }
var kBluetoothFeaturePowerControlRequests: BluetoothFeatureBits { get }
var kBluetoothFeatureChannelQuality: BluetoothFeatureBits { get }
var kBluetoothFeatureSCOLink: BluetoothFeatureBits { get }
var kBluetoothFeatureHV2Packets: BluetoothFeatureBits { get }
var kBluetoothFeatureHV3Packets: BluetoothFeatureBits { get }
var kBluetoothFeatureULawLog: BluetoothFeatureBits { get }
var kBluetoothFeatureALawLog: BluetoothFeatureBits { get }
var kBluetoothFeatureCVSD: BluetoothFeatureBits { get }
var kBluetoothFeaturePagingScheme: BluetoothFeatureBits { get }
var kBluetoothFeaturePowerControl: BluetoothFeatureBits { get }
var kBluetoothFeatureTransparentSCOData: BluetoothFeatureBits { get }
var kBluetoothFeatureFlowControlLagBit0: BluetoothFeatureBits { get }
var kBluetoothFeatureFlowControlLagBit1: BluetoothFeatureBits { get }
var kBluetoothFeatureFlowControlLagBit2: BluetoothFeatureBits { get }
var kBluetoothFeatureBroadcastEncryption: BluetoothFeatureBits { get }
var kBluetoothFeatureScatterMode: BluetoothFeatureBits { get }
var kBluetoothFeatureEnhancedDataRateACL2MbpsMode: BluetoothFeatureBits { get }
var kBluetoothFeatureEnhancedDataRateACL3MbpsMode: BluetoothFeatureBits { get }
var kBluetoothFeatureEnhancedInquiryScan: BluetoothFeatureBits { get }
var kBluetoothFeatureInterlacedInquiryScan: BluetoothFeatureBits { get }
var kBluetoothFeatureInterlacedPageScan: BluetoothFeatureBits { get }
var kBluetoothFeatureRSSIWithInquiryResult: BluetoothFeatureBits { get }
var kBluetoothFeatureExtendedSCOLink: BluetoothFeatureBits { get }
var kBluetoothFeatureEV4Packets: BluetoothFeatureBits { get }
var kBluetoothFeatureEV5Packets: BluetoothFeatureBits { get }
var kBluetoothFeatureAbsenceMasks: BluetoothFeatureBits { get }
var kBluetoothFeatureAFHCapableSlave: BluetoothFeatureBits { get }
var kBluetoothFeatureAFHClassificationSlave: BluetoothFeatureBits { get }
var kBluetoothFeatureAliasAuhentication: BluetoothFeatureBits { get }
var kBluetoothFeatureLESupportedController: BluetoothFeatureBits { get }
var kBluetoothFeature3SlotEnhancedDataRateACLPackets: BluetoothFeatureBits { get }
var kBluetoothFeature5SlotEnhancedDataRateACLPackets: BluetoothFeatureBits { get }
var kBluetoothFeatureSniffSubrating: BluetoothFeatureBits { get }
var kBluetoothFeaturePauseEncryption: BluetoothFeatureBits { get }
var kBluetoothFeatureAFHCapableMaster: BluetoothFeatureBits { get }
var kBluetoothFeatureAFHClassificationMaster: BluetoothFeatureBits { get }
var kBluetoothFeatureEnhancedDataRateeSCO2MbpsMode: BluetoothFeatureBits { get }
var kBluetoothFeatureEnhancedDataRateeSCO3MbpsMode: BluetoothFeatureBits { get }
var kBluetoothFeature3SlotEnhancedDataRateeSCOPackets: BluetoothFeatureBits { get }
var kBluetoothFeatureExtendedInquiryResponse: BluetoothFeatureBits { get }
var kBluetoothFeatureSecureSimplePairing: BluetoothFeatureBits { get }
var kBluetoothFeatureEncapsulatedPDU: BluetoothFeatureBits { get }
var kBluetoothFeatureErroneousDataReporting: BluetoothFeatureBits { get }
var kBluetoothFeatureNonFlushablePacketBoundaryFlag: BluetoothFeatureBits { get }
var kBluetoothFeatureLinkSupervisionTimeoutChangedEvent: BluetoothFeatureBits { get }
var kBluetoothFeatureInquiryTransmissionPowerLevel: BluetoothFeatureBits { get }
var kBluetoothFeatureExtendedFeatures: BluetoothFeatureBits { get }
var kBluetoothFeatureSimpleSecurePairingHostMode: BluetoothFeatureBits { get }
struct BluetoothEventFilterCondition {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothHCIFailedContactCount = UInt16
struct BluetoothHCIFailedContactInfo {
  var count: BluetoothHCIFailedContactCount
  var handle: BluetoothConnectionHandle
  init()
  init(count count: BluetoothHCIFailedContactCount, handle handle: BluetoothConnectionHandle)
}
typealias BluetoothHCIRSSIValue = Int8
struct BluetoothHCIRSSIInfo {
  var handle: BluetoothConnectionHandle
  var RSSIValue: BluetoothHCIRSSIValue
  init()
  init(handle handle: BluetoothConnectionHandle, RSSIValue RSSIValue: BluetoothHCIRSSIValue)
}
typealias BluetoothHCILinkQuality = UInt8
struct BluetoothHCILinkQualityInfo {
  var handle: BluetoothConnectionHandle
  var qualityValue: BluetoothHCILinkQuality
  init()
  init(handle handle: BluetoothConnectionHandle, qualityValue qualityValue: BluetoothHCILinkQuality)
}
typealias BluetoothHCIRole = UInt8
struct BluetoothHCIRoleInfo {
  var role: UInt8
  var handle: BluetoothConnectionHandle
  init()
  init(role role: UInt8, handle handle: BluetoothConnectionHandle)
}
struct BluetoothHCIRoles : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIMasterRole: BluetoothHCIRoles { get }
var kBluetoothHCISlaveRole: BluetoothHCIRoles { get }
typealias BluetoothHCILinkPolicySettings = UInt16
struct BluetoothHCILinkPolicySettingsValues : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kDisableAllLMModes: BluetoothHCILinkPolicySettingsValues { get }
var kEnableMasterSlaveSwitch: BluetoothHCILinkPolicySettingsValues { get }
var kEnableHoldMode: BluetoothHCILinkPolicySettingsValues { get }
var kEnableSniffMode: BluetoothHCILinkPolicySettingsValues { get }
var kEnableParkMode: BluetoothHCILinkPolicySettingsValues { get }
var kReservedForFutureUse: BluetoothHCILinkPolicySettingsValues { get }
struct BluetoothHCILinkPolicySettingsInfo {
  var settings: BluetoothHCILinkPolicySettings
  var handle: BluetoothConnectionHandle
  init()
  init(settings settings: BluetoothHCILinkPolicySettings, handle handle: BluetoothConnectionHandle)
}
struct BluetoothHCIQualityOfServiceSetupParams {
  var flags: UInt8
  var serviceType: UInt8
  var tokenRate: UInt32
  var peakBandwidth: UInt32
  var latency: UInt32
  var delayVariation: UInt32
  init()
  init(flags flags: UInt8, serviceType serviceType: UInt8, tokenRate tokenRate: UInt32, peakBandwidth peakBandwidth: UInt32, latency latency: UInt32, delayVariation delayVariation: UInt32)
}
struct BluetoothHCISetupSynchronousConnectionParams {
  var transmitBandwidth: UInt32
  var receiveBandwidth: UInt32
  var maxLatency: UInt16
  var voiceSetting: UInt16
  var retransmissionEffort: UInt8
  var packetType: UInt16
  init()
  init(transmitBandwidth transmitBandwidth: UInt32, receiveBandwidth receiveBandwidth: UInt32, maxLatency maxLatency: UInt16, voiceSetting voiceSetting: UInt16, retransmissionEffort retransmissionEffort: UInt8, packetType packetType: UInt16)
}
struct BluetoothHCIAcceptSynchronousConnectionRequestParams {
  var transmitBandwidth: UInt32
  var receiveBandwidth: UInt32
  var maxLatency: UInt16
  var contentFormat: UInt16
  var retransmissionEffort: UInt8
  var packetType: UInt16
  init()
  init(transmitBandwidth transmitBandwidth: UInt32, receiveBandwidth receiveBandwidth: UInt32, maxLatency maxLatency: UInt16, contentFormat contentFormat: UInt16, retransmissionEffort retransmissionEffort: UInt8, packetType packetType: UInt16)
}
typealias BluetoothHCILoopbackMode = UInt8
var kBluetoothHCILoopbackModeOff: Int { get }
var kBluetoothHCILoopbackModeLocal: Int { get }
var kBluetoothHCILoopbackModeRemote: Int { get }
struct BluetoothReadClockInfo {
  var handle: BluetoothConnectionHandle
  var clock: UInt32
  var accuracy: UInt16
  init()
  init(handle handle: BluetoothConnectionHandle, clock clock: UInt32, accuracy accuracy: UInt16)
}
struct BluetoothHCIEventFlowSpecificationData {
  var connectionHandle: BluetoothConnectionHandle
  var flags: UInt8
  var flowDirection: UInt8
  var serviceType: UInt8
  var tokenRate: UInt32
  var tokenBucketSize: UInt32
  var peakBandwidth: UInt32
  var accessLatency: UInt32
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, flags flags: UInt8, flowDirection flowDirection: UInt8, serviceType serviceType: UInt8, tokenRate tokenRate: UInt32, tokenBucketSize tokenBucketSize: UInt32, peakBandwidth peakBandwidth: UInt32, accessLatency accessLatency: UInt32)
}
typealias BluetoothHCIOperationID = UInt32
typealias BluetoothHCIEventID = UInt32
typealias BluetoothHCIDataID = UInt32
typealias BluetoothHCISignalID = UInt32
typealias BluetoothHCITransportID = UInt32
typealias BluetoothHCITransportCommandID = UInt32
typealias BluetoothHCIRequestID = UInt32
struct BluetoothHCIVersionInfo {
  var manufacturerName: BluetoothManufacturerName
  var lmpVersion: BluetoothLMPVersion
  var lmpSubVersion: BluetoothLMPSubversion
  var hciVersion: UInt8
  var hciRevision: UInt16
  init()
  init(manufacturerName manufacturerName: BluetoothManufacturerName, lmpVersion lmpVersion: BluetoothLMPVersion, lmpSubVersion lmpSubVersion: BluetoothLMPSubversion, hciVersion hciVersion: UInt8, hciRevision hciRevision: UInt16)
}
struct BluetoothHCIBufferSize {
  var ACLDataPacketLength: UInt16
  var SCODataPacketLength: UInt8
  var totalNumACLDataPackets: UInt16
  var totalNumSCODataPackets: UInt16
  init()
  init(ACLDataPacketLength ACLDataPacketLength: UInt16, SCODataPacketLength SCODataPacketLength: UInt8, totalNumACLDataPackets totalNumACLDataPackets: UInt16, totalNumSCODataPackets totalNumSCODataPackets: UInt16)
}
struct BluetoothHCILEBufferSize {
  var ACLDataPacketLength: UInt16
  var totalNumACLDataPackets: UInt8
  init()
  init(ACLDataPacketLength ACLDataPacketLength: UInt16, totalNumACLDataPackets totalNumACLDataPackets: UInt8)
}
typealias BluetoothHCIConnectionAcceptTimeout = UInt16
typealias BluetoothHCIPageTimeout = UInt16
struct BluetoothHCITimeoutValues : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kDefaultPageTimeout: BluetoothHCITimeoutValues { get }
typealias BluetoothHCINumLinkKeysDeleted = UInt16
typealias BluetoothHCINumLinkKeysToWrite = UInt8
typealias BluetoothHCIDeleteStoredLinkKeyFlag = UInt8
struct BluetoothHCIDeleteStoredLinkKeyFlags : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kDeleteKeyForSpecifiedDeviceOnly: BluetoothHCIDeleteStoredLinkKeyFlags { get }
var kDeleteAllStoredLinkKeys: BluetoothHCIDeleteStoredLinkKeyFlags { get }
typealias BluetoothHCIReadStoredLinkKeysFlag = UInt8
struct BluetoothHCIReadStoredLinkKeysFlags : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kReturnLinkKeyForSpecifiedDeviceOnly: BluetoothHCIReadStoredLinkKeysFlags { get }
var kReadAllStoredLinkKeys: BluetoothHCIReadStoredLinkKeysFlags { get }
struct BluetoothHCIStoredLinkKeysInfo {
  var numLinkKeysRead: UInt16
  var maxNumLinkKeysAllowedInDevice: UInt16
  init()
  init(numLinkKeysRead numLinkKeysRead: UInt16, maxNumLinkKeysAllowedInDevice maxNumLinkKeysAllowedInDevice: UInt16)
}
typealias BluetoothHCIPageScanMode = UInt8
struct BluetoothHCIPageScanModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kMandatoryPageScanMode: BluetoothHCIPageScanModes { get }
var kOptionalPageScanMode1: BluetoothHCIPageScanModes { get }
var kOptionalPageScanMode2: BluetoothHCIPageScanModes { get }
var kOptionalPageScanMode3: BluetoothHCIPageScanModes { get }
typealias BluetoothHCIPageScanPeriodMode = UInt8
struct BluetoothHCIPageScanPeriodModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kP0Mode: BluetoothHCIPageScanPeriodModes { get }
var kP1Mode: BluetoothHCIPageScanPeriodModes { get }
var kP2Mode: BluetoothHCIPageScanPeriodModes { get }
typealias BluetoothHCIPageScanEnableState = UInt8
struct BluetoothHCIPageScanEnableStates : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kNoScansEnabled: BluetoothHCIPageScanEnableStates { get }
var kInquiryScanEnabledPageScanDisabled: BluetoothHCIPageScanEnableStates { get }
var kInquiryScanDisabledPageScanEnabled: BluetoothHCIPageScanEnableStates { get }
var kInquiryScanEnabledPageScanEnabled: BluetoothHCIPageScanEnableStates { get }
struct BluetoothHCIScanActivity {
  var scanInterval: UInt16
  var scanWindow: UInt16
  init()
  init(scanInterval scanInterval: UInt16, scanWindow scanWindow: UInt16)
}
struct BluetoothHCIInquiryAccessCode {
  var data: (UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8))
}
typealias BluetoothHCIInquiryAccessCodeCount = UInt8
struct BluetoothHCICurrentInquiryAccessCodes {
  var count: BluetoothHCIInquiryAccessCodeCount
  var codes: UnsafeMutablePointer<BluetoothHCIInquiryAccessCode>!
  init()
  init(count count: BluetoothHCIInquiryAccessCodeCount, codes codes: UnsafeMutablePointer<BluetoothHCIInquiryAccessCode>!)
}
var kMaximumNumberOfInquiryAccessCodes: Int { get }
struct BluetoothHCICurrentInquiryAccessCodesForWrite {
  var count: BluetoothHCIInquiryAccessCodeCount
  var codes: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(count count: BluetoothHCIInquiryAccessCodeCount, codes codes: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCILinkSupervisionTimeout {
  var handle: BluetoothConnectionHandle
  var timeout: UInt16
  init()
  init(handle handle: BluetoothConnectionHandle, timeout timeout: UInt16)
}
typealias BluetoothHCIFlowControlState = UInt8
struct BluetoothHCISCOFlowControlStates : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kSCOFlowControlDisabled: BluetoothHCISCOFlowControlStates { get }
var kSCOFlowControlEnabled: BluetoothHCISCOFlowControlStates { get }
struct BluetoothHCIGeneralFlowControlStates : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kHostControllerToHostFlowControlOff: BluetoothHCIGeneralFlowControlStates { get }
var kHCIACLDataPacketsOnHCISCODataPacketsOff: BluetoothHCIGeneralFlowControlStates { get }
var kHCIACLDataPacketsOffHCISCODataPacketsOn: BluetoothHCIGeneralFlowControlStates { get }
var kHCIACLDataPacketsOnHCISCODataPacketsOn: BluetoothHCIGeneralFlowControlStates { get }
typealias BluetoothHCITransmitPowerLevel = Int8
typealias BluetoothHCITransmitPowerLevelType = UInt8
struct BluetoothHCITransmitReadPowerLevelTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kReadCurrentTransmitPowerLevel: BluetoothHCITransmitReadPowerLevelTypes { get }
var kReadMaximumTransmitPowerLevel: BluetoothHCITransmitReadPowerLevelTypes { get }
typealias BluetoothHCIAFHChannelAssessmentMode = UInt8
struct BluetoothHCIAFHChannelAssessmentModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kAFHChannelAssessmentModeDisabled: BluetoothHCIAFHChannelAssessmentModes { get }
var kAFHChannelAssessmentModeEnabled: BluetoothHCIAFHChannelAssessmentModes { get }
struct BluetoothHCITransmitPowerLevelInfo {
  var handle: BluetoothConnectionHandle
  var level: BluetoothHCITransmitPowerLevel
  init()
  init(handle handle: BluetoothConnectionHandle, level level: BluetoothHCITransmitPowerLevel)
}
typealias BluetoothHCINumBroadcastRetransmissions = UInt8
typealias BluetoothHCIHoldModeActivity = UInt8
struct BluetoothHCIHoldModeActivityStates : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kMaintainCurrentPowerState: BluetoothHCIHoldModeActivityStates { get }
var kSuspendPageScan: BluetoothHCIHoldModeActivityStates { get }
var kSuspendInquiryScan: BluetoothHCIHoldModeActivityStates { get }
var kSuspendPeriodicInquiries: BluetoothHCIHoldModeActivityStates { get }
typealias BluetoothHCIAuthenticationEnable = UInt8
struct BluetoothHCIAuthentionEnableModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kAuthenticationDisabled: BluetoothHCIAuthentionEnableModes { get }
var kAuthenticationEnabled: BluetoothHCIAuthentionEnableModes { get }
typealias BluetoothHCIEncryptionMode = UInt8
struct BluetoothHCIEncryptionModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kEncryptionDisabled: BluetoothHCIEncryptionModes { get }
var kEncryptionOnlyForPointToPointPackets: BluetoothHCIEncryptionModes { get }
var kEncryptionForBothPointToPointAndBroadcastPackets: BluetoothHCIEncryptionModes { get }
typealias BluetoothHCIAutomaticFlushTimeout = UInt16
struct BluetoothHCIAutomaticFlushTimeoutInfo {
  var handle: BluetoothConnectionHandle
  var timeout: BluetoothHCIAutomaticFlushTimeout
  init()
  init(handle handle: BluetoothConnectionHandle, timeout timeout: BluetoothHCIAutomaticFlushTimeout)
}
var kInfoStringMaxLength: Int32 { get }
typealias BluetoothTransportInfoPtr = UnsafeMutablePointer<BluetoothTransportInfo>
struct BluetoothTransportInfo {
  var productID: UInt32
  var vendorID: UInt32
  var type: UInt32
  var productName: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  var vendorName: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
  var totalDataBytesSent: UInt64
  var totalSCOBytesSent: UInt64
  var totalDataBytesReceived: UInt64
  var totalSCOBytesReceived: UInt64
  init()
  init(productID productID: UInt32, vendorID vendorID: UInt32, type type: UInt32, productName productName: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8), vendorName vendorName: (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8), totalDataBytesSent totalDataBytesSent: UInt64, totalSCOBytesSent totalSCOBytesSent: UInt64, totalDataBytesReceived totalDataBytesReceived: UInt64, totalSCOBytesReceived totalSCOBytesReceived: UInt64)
}
struct BluetoothTransportTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothTransportTypeUSB: BluetoothTransportTypes { get }
var kBluetoothTransportTypePCCard: BluetoothTransportTypes { get }
var kBluetoothTransportTypePCICard: BluetoothTransportTypes { get }
var kBluetoothTransportTypeUART: BluetoothTransportTypes { get }
struct BluetoothHCIInquiryResult {
  var deviceAddress: BluetoothDeviceAddress
  var pageScanRepetitionMode: BluetoothPageScanRepetitionMode
  var pageScanPeriodMode: BluetoothHCIPageScanPeriodMode
  var pageScanMode: BluetoothHCIPageScanMode
  var classOfDevice: BluetoothClassOfDevice
  var clockOffset: BluetoothClockOffset
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, pageScanRepetitionMode pageScanRepetitionMode: BluetoothPageScanRepetitionMode, pageScanPeriodMode pageScanPeriodMode: BluetoothHCIPageScanPeriodMode, pageScanMode pageScanMode: BluetoothHCIPageScanMode, classOfDevice classOfDevice: BluetoothClassOfDevice, clockOffset clockOffset: BluetoothClockOffset)
}
var kBluetoothHCIInquiryResultsMaxResults: Int32 { get }
struct BluetoothHCIInquiryResults {
  var results: (BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult)
  var count: IOItemCount
  init()
  init(results results: (BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult, BluetoothHCIInquiryResult), count count: IOItemCount)
}
struct BluetoothHCIInquiryWithRSSIResult {
  var deviceAddress: BluetoothDeviceAddress
  var pageScanRepetitionMode: BluetoothPageScanRepetitionMode
  var reserved: UInt8
  var classOfDevice: BluetoothClassOfDevice
  var clockOffset: BluetoothClockOffset
  var RSSIValue: BluetoothHCIRSSIValue
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, pageScanRepetitionMode pageScanRepetitionMode: BluetoothPageScanRepetitionMode, reserved reserved: UInt8, classOfDevice classOfDevice: BluetoothClassOfDevice, clockOffset clockOffset: BluetoothClockOffset, RSSIValue RSSIValue: BluetoothHCIRSSIValue)
}
struct BluetoothHCIInquiryWithRSSIResults {
  var results: (BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult)
  var count: IOItemCount
  init()
  init(results results: (BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult, BluetoothHCIInquiryWithRSSIResult), count count: IOItemCount)
}
typealias BluetoothHCIFECRequired = UInt8
struct BluetoothHCIFECRequiredValues : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIFECRequired: BluetoothHCIFECRequiredValues { get }
var kBluetoothHCIFECNotRequired: BluetoothHCIFECRequiredValues { get }
typealias BluetoothHCIInquiryMode = UInt8
struct BluetoothHCIInquiryModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIInquiryModeResultFormatStandard: BluetoothHCIInquiryModes { get }
var kBluetoothHCIInquiryModeResultFormatWithRSSI: BluetoothHCIInquiryModes { get }
var kBluetoothHCIInquiryModeResultFormatWithRSSIOrExtendedInquiryResultFormat: BluetoothHCIInquiryModes { get }
typealias BluetoothHCIInquiryScanType = UInt8
struct BluetoothHCIInquiryScanTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIInquiryScanTypeStandard: BluetoothHCIInquiryScanTypes { get }
var kBluetoothHCIInquiryScanTypeInterlaced: BluetoothHCIInquiryScanTypes { get }
var kBluetoothHCIInquiryScanTypeReservedStart: BluetoothHCIInquiryScanTypes { get }
var kBluetoothHCIInquiryScanTypeReservedEnd: BluetoothHCIInquiryScanTypes { get }
typealias BluetoothHCIExtendedInquiryResponseDataType = UInt8
struct BluetoothHCIExtendedInquiryResponse {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCIReadExtendedInquiryResponseResults {
  var outFECRequired: BluetoothHCIFECRequired
  var extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse
  init()
  init(outFECRequired outFECRequired: BluetoothHCIFECRequired, extendedInquiryResponse extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse)
}
struct BluetoothHCIExtendedInquiryResult {
  var numberOfReponses: UInt8
  var deviceAddress: BluetoothDeviceAddress
  var pageScanRepetitionMode: BluetoothPageScanRepetitionMode
  var reserved: UInt8
  var classOfDevice: BluetoothClassOfDevice
  var clockOffset: BluetoothClockOffset
  var RSSIValue: BluetoothHCIRSSIValue
  var extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse
  init()
  init(numberOfReponses numberOfReponses: UInt8, deviceAddress deviceAddress: BluetoothDeviceAddress, pageScanRepetitionMode pageScanRepetitionMode: BluetoothPageScanRepetitionMode, reserved reserved: UInt8, classOfDevice classOfDevice: BluetoothClassOfDevice, clockOffset clockOffset: BluetoothClockOffset, RSSIValue RSSIValue: BluetoothHCIRSSIValue, extendedInquiryResponse extendedInquiryResponse: BluetoothHCIExtendedInquiryResponse)
}
struct BluetoothHCIReadLMPHandleResults {
  var handle: BluetoothConnectionHandle
  var lmp_handle: BluetoothLMPHandle
  var reserved: UInt32
  init()
  init(handle handle: BluetoothConnectionHandle, lmp_handle lmp_handle: BluetoothLMPHandle, reserved reserved: UInt32)
}
typealias BluetoothHCISimplePairingMode = UInt8
struct BluetoothHCISimplePairingModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCISimplePairingModeNotSet: BluetoothHCISimplePairingModes { get }
var kBluetoothHCISimplePairingModeEnabled: BluetoothHCISimplePairingModes { get }
typealias BluetoothSimplePairingDebugMode = UInt8
struct BluetoothSimplePairingDebugModes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCISimplePairingDebugModeDisabled: BluetoothSimplePairingDebugModes { get }
var kBluetoothHCISimplePairingDebugModeEnabled: BluetoothSimplePairingDebugModes { get }
struct BluetoothHCISimplePairingOOBData {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCIReadLocalOOBDataResults {
  var hash: BluetoothHCISimplePairingOOBData
  var randomizer: BluetoothHCISimplePairingOOBData
  init()
  init(hash hash: BluetoothHCISimplePairingOOBData, randomizer randomizer: BluetoothHCISimplePairingOOBData)
}
typealias BluetoothIOCapability = UInt8
struct BluetoothIOCapabilities : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothCapabilityTypeDisplayOnly: BluetoothIOCapabilities { get }
var kBluetoothCapabilityTypeDisplayYesNo: BluetoothIOCapabilities { get }
var kBluetoothCapabilityTypeKeyboardOnly: BluetoothIOCapabilities { get }
var kBluetoothCapabilityTypeNoInputNoOutput: BluetoothIOCapabilities { get }
typealias BluetoothOOBDataPresence = UInt8
struct BluetoothOOBDataPresenceValues : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothOOBAuthenticationDataNotPresent: BluetoothOOBDataPresenceValues { get }
var kBluetoothOOBAuthenticationDataFromRemoteDevicePresent: BluetoothOOBDataPresenceValues { get }
typealias BluetoothAuthenticationRequirements = UInt8
struct BluetoothAuthenticationRequirementsValues : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothAuthenticationRequirementsMITMProtectionNotRequired: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionRequired: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredNoBonding: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionRequiredNoBonding: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredDedicatedBonding: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionRequiredDedicatedBonding: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionNotRequiredGeneralBonding: BluetoothAuthenticationRequirementsValues { get }
var kBluetoothAuthenticationRequirementsMITMProtectionRequiredGeneralBonding: BluetoothAuthenticationRequirementsValues { get }
struct BluetoothIOCapabilityResponse {
  var deviceAddress: BluetoothDeviceAddress
  var ioCapability: BluetoothIOCapability
  var OOBDataPresence: BluetoothOOBDataPresence
  var authenticationRequirements: BluetoothAuthenticationRequirements
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, ioCapability ioCapability: BluetoothIOCapability, OOBDataPresence OOBDataPresence: BluetoothOOBDataPresence, authenticationRequirements authenticationRequirements: BluetoothAuthenticationRequirements)
}
typealias BluetoothPasskey = UInt32
struct BluetoothUserPasskeyNotification {
  var deviceAddress: BluetoothDeviceAddress
  var passkey: BluetoothPasskey
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, passkey passkey: BluetoothPasskey)
}
typealias BluetoothKeypressNotificationType = UInt8
struct BluetoothKeypressNotificationTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothKeypressNotificationTypePasskeyEntryStarted: BluetoothKeypressNotificationTypes { get }
var kBluetoothKeypressNotificationTypePasskeyDigitEntered: BluetoothKeypressNotificationTypes { get }
var kBluetoothKeypressNotificationTypePasskeyDigitErased: BluetoothKeypressNotificationTypes { get }
var kBluetoothKeypressNotificationTypePasskeyCleared: BluetoothKeypressNotificationTypes { get }
var kBluetoothKeypressNotificationTypePasskeyEntryCompleted: BluetoothKeypressNotificationTypes { get }
struct BluetoothKeypressNotification {
  var deviceAddress: BluetoothDeviceAddress
  var notificationType: BluetoothKeypressNotificationType
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, notificationType notificationType: BluetoothKeypressNotificationType)
}
struct BluetoothRemoteHostSupportedFeaturesNotification {
  var deviceAddress: BluetoothDeviceAddress
  var hostSupportedFeatures: BluetoothHCISupportedFeatures
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, hostSupportedFeatures hostSupportedFeatures: BluetoothHCISupportedFeatures)
}
typealias TransmissionPower = Int8
struct BluetoothAFHHostChannelClassification {
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothAFHMode = UInt8
struct BluetoothAFHResults {
  var handle: BluetoothConnectionHandle
  var mode: BluetoothAFHMode
  var afhMap: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(handle handle: BluetoothConnectionHandle, mode mode: BluetoothAFHMode, afhMap afhMap: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
typealias BluetoothNumericValue = UInt32
struct BluetoothUserConfirmationRequest {
  var deviceAddress: BluetoothDeviceAddress
  var numericValue: BluetoothNumericValue
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, numericValue numericValue: BluetoothNumericValue)
}
struct BluetoothHCIEventSimplePairingCompleteResults {
  var deviceAddress: BluetoothDeviceAddress
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress)
}
var kBluetoothHCICommandPacketHeaderSize: Int { get }
var kBluetoothHCICommandPacketMaxDataSize: Int { get }
var kBluetoothHCIMaxCommandPacketSize: Int { get }
var kBluetoothHCIEventPacketHeaderSize: Int { get }
var kBluetoothHCIEventPacketMaxDataSize: Int { get }
var kBluetoothHCIMaxEventPacketSize: Int { get }
var kBluetoothHCIDataPacketHeaderSize: Int { get }
var kBluetoothHCIDataPacketMaxDataSize: Int { get }
var kBluetoothHCIMaxDataPacketSize: Int { get }
typealias BluetoothHCIEventCode = UInt8
typealias BluetoothLinkType = UInt8
struct BluetoothLinkTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothSCOConnection: BluetoothLinkTypes { get }
var kBluetoothACLConnection: BluetoothLinkTypes { get }
var kBluetoothESCOConnection: BluetoothLinkTypes { get }
var kBluetoothLinkTypeNone: BluetoothLinkTypes { get }
typealias BluetoothHCIContentFormat = UInt16
typealias BluetoothHCIVoiceSetting = UInt16
var kBluetoothVoiceSettingInputCodingMask: Int { get }
var kBluetoothVoiceSettingInputCodingLinearInputCoding: Int { get }
var kBluetoothVoiceSettingInputCodingULawInputCoding: Int { get }
var kBluetoothVoiceSettingInputCodingALawInputCoding: Int { get }
var kBluetoothVoiceSettingInputDataFormatMask: Int { get }
var kBluetoothVoiceSettingInputDataFormat1sComplement: Int { get }
var kBluetoothVoiceSettingInputDataFormat2sComplement: Int { get }
var kBluetoothVoiceSettingInputDataFormatSignMagnitude: Int { get }
var kBluetoothVoiceSettingInputDataFormatUnsigned: Int { get }
var kBluetoothVoiceSettingInputSampleSizeMask: Int { get }
var kBluetoothVoiceSettingInputSampleSize8Bit: Int { get }
var kBluetoothVoiceSettingInputSampleSize16Bit: Int { get }
var kBluetoothVoiceSettingPCMBitPositionMask: Int { get }
var kBluetoothVoiceSettingAirCodingFormatMask: Int { get }
var kBluetoothVoiceSettingAirCodingFormatCVSD: Int { get }
var kBluetoothVoiceSettingAirCodingFormatULaw: Int { get }
var kBluetoothVoiceSettingAirCodingFormatALaw: Int { get }
var kBluetoothVoiceSettingAirCodingFormatTransparentData: Int { get }
typealias BluetoothHCISupportedIAC = UInt8
typealias BluetoothHCITransmitBandwidth = UInt32
typealias BluetoothHCIReceiveBandwidth = UInt32
typealias BluetoothHCIMaxLatency = UInt16
typealias BluetoothHCIRetransmissionEffort = UInt8
struct BluetoothHCIRetransmissionEffortTypes : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kHCIRetransmissionEffortTypeNone: BluetoothHCIRetransmissionEffortTypes { get }
var kHCIRetransmissionEffortTypeAtLeastOneAndOptimizeForPower: BluetoothHCIRetransmissionEffortTypes { get }
var kHCIRetransmissionEffortTypeAtLeastOneAndOptimizeLinkQuality: BluetoothHCIRetransmissionEffortTypes { get }
var kHCIRetransmissionEffortTypeDontCare: BluetoothHCIRetransmissionEffortTypes { get }
typealias BluetoothAirMode = UInt8
var kBluetoothAirModeULawLog: Int { get }
var kBluetoothAirModeALawLog: Int { get }
var kBluetoothAirModeCVSD: Int { get }
var kBluetoothAirModeTransparentData: Int { get }
struct BluetoothSynchronousConnectionInfo {
  var transmitBandWidth: BluetoothHCITransmitBandwidth
  var receiveBandWidth: BluetoothHCIReceiveBandwidth
  var maxLatency: BluetoothHCIMaxLatency
  var voiceSetting: BluetoothHCIVoiceSetting
  var retransmissionEffort: BluetoothHCIRetransmissionEffort
  var packetType: BluetoothPacketType
  init()
  init(transmitBandWidth transmitBandWidth: BluetoothHCITransmitBandwidth, receiveBandWidth receiveBandWidth: BluetoothHCIReceiveBandwidth, maxLatency maxLatency: BluetoothHCIMaxLatency, voiceSetting voiceSetting: BluetoothHCIVoiceSetting, retransmissionEffort retransmissionEffort: BluetoothHCIRetransmissionEffort, packetType packetType: BluetoothPacketType)
}
struct BluetoothHCIEventSynchronousConnectionCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var deviceAddress: BluetoothDeviceAddress
  var linkType: BluetoothLinkType
  var transmissionInterval: UInt8
  var retransmissionWindow: UInt8
  var receivePacketLength: UInt16
  var transmitPacketLength: UInt16
  var airMode: BluetoothAirMode
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, deviceAddress deviceAddress: BluetoothDeviceAddress, linkType linkType: BluetoothLinkType, transmissionInterval transmissionInterval: UInt8, retransmissionWindow retransmissionWindow: UInt8, receivePacketLength receivePacketLength: UInt16, transmitPacketLength transmitPacketLength: UInt16, airMode airMode: BluetoothAirMode)
}
struct BluetoothHCIEventSynchronousConnectionChangedResults {
  var connectionHandle: BluetoothConnectionHandle
  var transmissionInterval: UInt8
  var retransmissionWindow: UInt8
  var receivePacketLength: UInt16
  var transmitPacketLength: UInt16
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, transmissionInterval transmissionInterval: UInt8, retransmissionWindow retransmissionWindow: UInt8, receivePacketLength receivePacketLength: UInt16, transmitPacketLength transmitPacketLength: UInt16)
}
typealias BluetoothHCIStatus = UInt8
typealias BluetoothHCIEventStatus = UInt8
var kBluetoothHCIEventInquiryComplete: Int { get }
var kBluetoothHCIEventInquiryResult: Int { get }
var kBluetoothHCIEventConnectionComplete: Int { get }
var kBluetoothHCIEventConnectionRequest: Int { get }
var kBluetoothHCIEventDisconnectionComplete: Int { get }
var kBluetoothHCIEventAuthenticationComplete: Int { get }
var kBluetoothHCIEventRemoteNameRequestComplete: Int { get }
var kBluetoothHCIEventEncryptionChange: Int { get }
var kBluetoothHCIEventChangeConnectionLinkKeyComplete: Int { get }
var kBluetoothHCIEventMasterLinkKeyComplete: Int { get }
var kBluetoothHCIEventReadRemoteSupportedFeaturesComplete: Int { get }
var kBluetoothHCIEventReadRemoteVersionInformationComplete: Int { get }
var kBluetoothHCIEventQoSSetupComplete: Int { get }
var kBluetoothHCIEventCommandComplete: Int { get }
var kBluetoothHCIEventCommandStatus: Int { get }
var kBluetoothHCIEventHardwareError: Int { get }
var kBluetoothHCIEventFlushOccurred: Int { get }
var kBluetoothHCIEventRoleChange: Int { get }
var kBluetoothHCIEventNumberOfCompletedPackets: Int { get }
var kBluetoothHCIEventModeChange: Int { get }
var kBluetoothHCIEventReturnLinkKeys: Int { get }
var kBluetoothHCIEventPINCodeRequest: Int { get }
var kBluetoothHCIEventLinkKeyRequest: Int { get }
var kBluetoothHCIEventLinkKeyNotification: Int { get }
var kBluetoothHCIEventLoopbackCommand: Int { get }
var kBluetoothHCIEventDataBufferOverflow: Int { get }
var kBluetoothHCIEventMaxSlotsChange: Int { get }
var kBluetoothHCIEventReadClockOffsetComplete: Int { get }
var kBluetoothHCIEventConnectionPacketType: Int { get }
var kBluetoothHCIEventQoSViolation: Int { get }
var kBluetoothHCIEventPageScanModeChange: Int { get }
var kBluetoothHCIEventPageScanRepetitionModeChange: Int { get }
var kBluetoothHCIEventFlowSpecificationComplete: Int { get }
var kBluetoothHCIEventInquiryResultWithRSSI: Int { get }
var kBluetoothHCIEventReadRemoteExtendedFeaturesComplete: Int { get }
var kBluetoothHCIEventSynchronousConnectionComplete: Int { get }
var kBluetoothHCIEventSynchronousConnectionChanged: Int { get }
var kBluetoothHCIEventSniffSubrating: Int { get }
var kBluetoothHCIEventExtendedInquiryResult: Int { get }
var kBluetoothHCIEventEncryptionKeyRefreshComplete: Int { get }
var kBluetoothHCIEventIOCapabilityRequest: Int { get }
var kBluetoothHCIEventIOCapabilityResponse: Int { get }
var kBluetoothHCIEventUserConfirmationRequest: Int { get }
var kBluetoothHCIEventUserPasskeyRequest: Int { get }
var kBluetoothHCIEventRemoteOOBDataRequest: Int { get }
var kBluetoothHCIEventSimplePairingComplete: Int { get }
var kBluetoothHCIEventLinkSupervisionTimeoutChanged: Int { get }
var kBluetoothHCIEventEnhancedFlushComplete: Int { get }
var kBluetoothHCIEventUserPasskeyNotification: Int { get }
var kBluetoothHCIEventKeypressNotification: Int { get }
var kBluetoothHCIEventRemoteHostSupportedFeaturesNotification: Int { get }
var kBluetoothHCIEventLEMetaEvent: Int { get }
var kBluetoothHCISubEventLEConnectionComplete: Int { get }
var kBluetoothHCISubEventLEAdvertisingReport: Int { get }
var kBluetoothHCISubEventLEConnectionUpdateComplete: Int { get }
var kBluetoothHCISubEventLEReadRemoteUsedFeaturesComplete: Int { get }
var kBluetoothHCISubEventLELongTermKeyRequest: Int { get }
var kBluetoothHCIEventPhysicalLinkComplete: Int { get }
var kBluetoothHCIEventChannelSelected: Int { get }
var kBluetoothHCIEventDisconnectionPhysicalLinkComplete: Int { get }
var kBluetoothHCIEventPhysicalLinkLossEarlyWarning: Int { get }
var kBluetoothHCIEventPhysicalLinkRecovery: Int { get }
var kBluetoothHCIEventLogicalLinkComplete: Int { get }
var kBluetoothHCIEventDisconnectionLogicalLinkComplete: Int { get }
var kBluetoothHCIEventFlowSpecModifyComplete: Int { get }
var kBluetoothHCIEventNumberOfCompletedDataBlocks: Int { get }
var kBluetoothHCIEventShortRangeModeChangeComplete: Int { get }
var kBluetoothHCIEventAMPStatusChange: Int { get }
var kBluetoothHCIEventAMPStartTest: Int { get }
var kBluetoothHCIEventAMPTestEnd: Int { get }
var kBluetoothHCIEventAMPReceiverReport: Int { get }
var kBluetoothHCIEventLogoTesting: Int { get }
var kBluetoothHCIEventVendorSpecific: Int { get }
typealias BluetoothHCIEventMask = UInt64
var kBluetoothHCIEventMaskLEDefault64Bit: Int64 { get }
var kBluetoothHCIEventMaskDefault64Bit: Int64 { get }
var kBluetoothHCIEventMaskAll64Bit: UInt64 { get }
var kBluetoothHCIEventMaskFlowSpecificationCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskInquiryResultWithRSSIEvent: Int64 { get }
var kBluetoothHCIEventMaskReadRemoteExtendedFeaturesCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskSynchronousConnectionCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskSynchronousConnectionChangedEvent: Int64 { get }
var kBluetoothHCIEventMaskSniffSubratingEvent: Int64 { get }
var kBluetoothHCIEventMaskExtendedInquiryResultEvent: Int64 { get }
var kBluetoothHCIEventMaskEncryptionChangeEvent: Int64 { get }
var kBluetoothHCIEventMaskEncryptionKeyRefreshCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskLinkSupervisionTimeoutChangedEvent: Int64 { get }
var kBluetoothHCIEventMaskEnhancedFlushCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskIOCapabilityRequestEvent: Int64 { get }
var kBluetoothHCIEventMaskIOCapabilityRequestReplyEvent: Int64 { get }
var kBluetoothHCIEventMaskUserConfirmationRequestEvent: Int64 { get }
var kBluetoothHCIEventMaskUserPasskeyRequestEvent: Int64 { get }
var kBluetoothHCIEventMaskRemoteOOBDataRequestEvent: Int64 { get }
var kBluetoothHCIEventMaskSimplePairingCompleteEvent: Int64 { get }
var kBluetoothHCIEvnetMaskLinkSupervisionTimeoutChangedEvent: Int64 { get }
var kBluetoothHCIEvnetMaskEnhancedFlushCompleteEvent: Int64 { get }
var kBluetoothHCIEventMaskUserPasskeyNotificationEvent: Int64 { get }
var kBluetoothHCIEventMaskKeypressNotificationEvent: Int64 { get }
var kBluetoothHCIEventMaskRemoteHostSupportedFeaturesNotificationEvent: Int64 { get }
var kBluetoothHCIEventMaskLEMetaEvent: Int64 { get }
var kBluetoothHCIEventMaskNone: UInt32 { get }
var kBluetoothHCIEventMaskInquiryComplete: UInt32 { get }
var kBluetoothHCIEventMaskInquiryResult: UInt32 { get }
var kBluetoothHCIEventMaskConnectionComplete: UInt32 { get }
var kBluetoothHCIEventMaskConnectionRequest: UInt32 { get }
var kBluetoothHCIEventMaskDisconnectionComplete: UInt32 { get }
var kBluetoothHCIEventMaskAuthenticationComplete: UInt32 { get }
var kBluetoothHCIEventMaskRemoteNameRequestComplete: UInt32 { get }
var kBluetoothHCIEventMaskEncryptionChange: UInt32 { get }
var kBluetoothHCIEventMaskChangeConnectionLinkKeyComplete: UInt32 { get }
var kBluetoothHCIEventMaskMasterLinkKeyComplete: UInt32 { get }
var kBluetoothHCIEventMaskReadRemoteSupportedFeaturesComplete: UInt32 { get }
var kBluetoothHCIEventMaskReadRemoteVersionInformationComplete: UInt32 { get }
var kBluetoothHCIEventMaskQoSSetupComplete: UInt32 { get }
var kBluetoothHCIEventMaskCommandComplete: UInt32 { get }
var kBluetoothHCIEventMaskCommandStatus: UInt32 { get }
var kBluetoothHCIEventMaskHardwareError: UInt32 { get }
var kBluetoothHCIEventMaskFlushOccurred: UInt32 { get }
var kBluetoothHCIEventMaskRoleChange: UInt32 { get }
var kBluetoothHCIEventMaskNumberOfCompletedPackets: UInt32 { get }
var kBluetoothHCIEventMaskModeChange: UInt32 { get }
var kBluetoothHCIEventMaskReturnLinkKeys: UInt32 { get }
var kBluetoothHCIEventMaskPINCodeRequest: UInt32 { get }
var kBluetoothHCIEventMaskLinkKeyRequest: UInt32 { get }
var kBluetoothHCIEventMaskLinkKeyNotification: UInt32 { get }
var kBluetoothHCIEventMaskLoopbackCommand: UInt32 { get }
var kBluetoothHCIEventMaskDataBufferOverflow: UInt32 { get }
var kBluetoothHCIEventMaskMaxSlotsChange: UInt32 { get }
var kBluetoothHCIEventMaskReadClockOffsetComplete: UInt32 { get }
var kBluetoothHCIEventMaskConnectionPacketTypeChanged: UInt32 { get }
var kBluetoothHCIEventMaskQoSViolation: UInt32 { get }
var kBluetoothHCIEventMaskPageScanModeChange: UInt32 { get }
var kBluetoothHCIEventMaskPageScanRepetitionModeChange: UInt32 { get }
var kBluetoothHCIEventMaskAll: UInt32 { get }
var kBluetoothHCIEventMaskDefault: UInt32 { get }
struct BluetoothHCIEventConnectionCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var deviceAddress: BluetoothDeviceAddress
  var linkType: BluetoothLinkType
  var encryptionMode: BluetoothHCIEncryptionMode
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, deviceAddress deviceAddress: BluetoothDeviceAddress, linkType linkType: BluetoothLinkType, encryptionMode encryptionMode: BluetoothHCIEncryptionMode)
}
struct BluetoothHCIEventLEConnectionCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var role: UInt8
  var peerAddressType: UInt8
  var peerAddress: BluetoothDeviceAddress
  var connInterval: UInt16
  var connLatency: UInt16
  var supervisionTimeout: UInt16
  var masterClockAccuracy: UInt8
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, role role: UInt8, peerAddressType peerAddressType: UInt8, peerAddress peerAddress: BluetoothDeviceAddress, connInterval connInterval: UInt16, connLatency connLatency: UInt16, supervisionTimeout supervisionTimeout: UInt16, masterClockAccuracy masterClockAccuracy: UInt8)
}
struct BluetoothHCIEventLEConnectionUpdateCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var connInterval: UInt16
  var connLatency: UInt16
  var supervisionTimeout: UInt16
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, connInterval connInterval: UInt16, connLatency connLatency: UInt16, supervisionTimeout supervisionTimeout: UInt16)
}
struct BluetoothHCIEventDisconnectionCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var reason: BluetoothReasonCode
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, reason reason: BluetoothReasonCode)
}
struct BluetoothHCIEventReadSupportedFeaturesResults {
  var connectionHandle: BluetoothConnectionHandle
  var supportedFeatures: BluetoothHCISupportedFeatures
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, supportedFeatures supportedFeatures: BluetoothHCISupportedFeatures)
}
struct BluetoothHCIEventReadExtendedFeaturesResults {
  var connectionHandle: BluetoothConnectionHandle
  var supportedFeaturesInfo: BluetoothHCIExtendedFeaturesInfo
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, supportedFeaturesInfo supportedFeaturesInfo: BluetoothHCIExtendedFeaturesInfo)
}
struct BluetoothHCIEventReadRemoteVersionInfoResults {
  var connectionHandle: BluetoothConnectionHandle
  var lmpVersion: BluetoothLMPVersion
  var manufacturerName: BluetoothManufacturerName
  var lmpSubversion: BluetoothLMPSubversion
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, lmpVersion lmpVersion: BluetoothLMPVersion, manufacturerName manufacturerName: BluetoothManufacturerName, lmpSubversion lmpSubversion: BluetoothLMPSubversion)
}
struct BluetoothHCIEventRemoteNameRequestResults {
  var deviceAddress: BluetoothDeviceAddress
  var deviceName: BluetoothDeviceName
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, deviceName deviceName: BluetoothDeviceName)
}
struct BluetoothHCIEventReadClockOffsetResults {
  var connectionHandle: BluetoothConnectionHandle
  var clockOffset: BluetoothClockOffset
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, clockOffset clockOffset: BluetoothClockOffset)
}
struct BluetoothHCIEventConnectionRequestResults {
  var deviceAddress: BluetoothDeviceAddress
  var classOfDevice: BluetoothClassOfDevice
  var linkType: BluetoothLinkType
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, classOfDevice classOfDevice: BluetoothClassOfDevice, linkType linkType: BluetoothLinkType)
}
struct BluetoothHCIEventLinkKeyNotificationResults {
  var deviceAddress: BluetoothDeviceAddress
  var linkKey: BluetoothKey
  var keyType: BluetoothKeyType
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, linkKey linkKey: BluetoothKey, keyType keyType: BluetoothKeyType)
}
struct BluetoothHCIEventMaxSlotsChangeResults {
  var connectionHandle: BluetoothConnectionHandle
  var maxSlots: BluetoothMaxSlots
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, maxSlots maxSlots: BluetoothMaxSlots)
}
struct BluetoothHCIEventModeChangeResults {
  var connectionHandle: BluetoothConnectionHandle
  var mode: BluetoothHCIConnectionMode
  var modeInterval: BluetoothHCIModeInterval
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, mode mode: BluetoothHCIConnectionMode, modeInterval modeInterval: BluetoothHCIModeInterval)
}
struct BluetoothHCIEventReturnLinkKeysResults {
  var numLinkKeys: UInt8
  init()
}
struct BluetoothHCIEventAuthenticationCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle)
}
struct BluetoothHCIEventEncryptionChangeResults {
  var connectionHandle: BluetoothConnectionHandle
  var enable: BluetoothEncryptionEnable
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, enable enable: BluetoothEncryptionEnable)
}
struct BluetoothHCIEventChangeConnectionLinkKeyCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle)
}
struct BluetoothHCIEventMasterLinkKeyCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var keyFlag: BluetoothKeyFlag
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, keyFlag keyFlag: BluetoothKeyFlag)
}
struct BluetoothHCIEventQoSSetupCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  var setupParams: BluetoothHCIQualityOfServiceSetupParams
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, setupParams setupParams: BluetoothHCIQualityOfServiceSetupParams)
}
struct BluetoothHCIEventHardwareErrorResults {
  var error: BluetoothHCIStatus
  init()
  init(error error: BluetoothHCIStatus)
}
struct BluetoothHCIEventFlushOccurredResults {
  var connectionHandle: BluetoothConnectionHandle
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle)
}
struct BluetoothHCIEventRoleChangeResults {
  var connectionHandle: BluetoothConnectionHandle
  var deviceAddress: BluetoothDeviceAddress
  var role: BluetoothRole
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, deviceAddress deviceAddress: BluetoothDeviceAddress, role role: BluetoothRole)
}
struct BluetoothHCIEventDataBufferOverflowResults {
  var linkType: BluetoothLinkType
  init()
  init(linkType linkType: BluetoothLinkType)
}
struct BluetoothHCIEventConnectionPacketTypeResults {
  var connectionHandle: BluetoothConnectionHandle
  var packetType: BluetoothPacketType
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, packetType packetType: BluetoothPacketType)
}
struct BluetoothHCIEventReadRemoteSupportedFeaturesResults {
  var error: BluetoothHCIStatus
  var connectionHandle: BluetoothConnectionHandle
  var lmpFeatures: BluetoothHCISupportedFeatures
  init()
  init(error error: BluetoothHCIStatus, connectionHandle connectionHandle: BluetoothConnectionHandle, lmpFeatures lmpFeatures: BluetoothHCISupportedFeatures)
}
struct BluetoothHCIEventReadRemoteExtendedFeaturesResults {
  var error: BluetoothHCIStatus
  var connectionHandle: BluetoothConnectionHandle
  var page: BluetoothHCIPageNumber
  var maxPage: BluetoothHCIPageNumber
  var lmpFeatures: BluetoothHCISupportedFeatures
  init()
  init(error error: BluetoothHCIStatus, connectionHandle connectionHandle: BluetoothConnectionHandle, page page: BluetoothHCIPageNumber, maxPage maxPage: BluetoothHCIPageNumber, lmpFeatures lmpFeatures: BluetoothHCISupportedFeatures)
}
struct BluetoothHCIEventQoSViolationResults {
  var connectionHandle: BluetoothConnectionHandle
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle)
}
struct BluetoothHCIEventPageScanModeChangeResults {
  var deviceAddress: BluetoothDeviceAddress
  var pageScanMode: BluetoothPageScanMode
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, pageScanMode pageScanMode: BluetoothPageScanMode)
}
struct BluetoothHCIEventPageScanRepetitionModeChangeResults {
  var deviceAddress: BluetoothDeviceAddress
  var pageScanRepetitionMode: BluetoothPageScanRepetitionMode
  init()
  init(deviceAddress deviceAddress: BluetoothDeviceAddress, pageScanRepetitionMode pageScanRepetitionMode: BluetoothPageScanRepetitionMode)
}
struct BluetoothHCIEventVendorSpecificResults {
  var length: UInt8
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(length length: UInt8, data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCIEventEncryptionKeyRefreshCompleteResults {
  var connectionHandle: BluetoothConnectionHandle
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle)
}
struct BluetoothHCIEventSniffSubratingResults {
  var connectionHandle: BluetoothConnectionHandle
  var maxTransmitLatency: UInt16
  var maxReceiveLatency: UInt16
  var minRemoteTimeout: UInt16
  var minLocalTimeout: UInt16
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, maxTransmitLatency maxTransmitLatency: UInt16, maxReceiveLatency maxReceiveLatency: UInt16, minRemoteTimeout minRemoteTimeout: UInt16, minLocalTimeout minLocalTimeout: UInt16)
}
struct BluetoothHCIEventLEMetaResults {
  var length: UInt8
  var data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  init()
  init(length length: UInt8, data data: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8))
}
struct BluetoothHCIEventLELongTermKeyRequestResults {
  var connectionHandle: BluetoothConnectionHandle
  var randomNumber: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var ediv: UInt16
  init()
  init(connectionHandle connectionHandle: BluetoothConnectionHandle, randomNumber randomNumber: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), ediv ediv: UInt16)
}
struct BluetoothHCIRequestCallbackInfo {
  var userCallback: mach_vm_address_t
  var userRefCon: mach_vm_address_t
  var internalRefCon: mach_vm_address_t
  var asyncIDRefCon: mach_vm_address_t
  var reserved: mach_vm_address_t
  init()
  init(userCallback userCallback: mach_vm_address_t, userRefCon userRefCon: mach_vm_address_t, internalRefCon internalRefCon: mach_vm_address_t, asyncIDRefCon asyncIDRefCon: mach_vm_address_t, reserved reserved: mach_vm_address_t)
}
var kBluetoothHCIErrorSuccess: Int { get }
var kBluetoothHCIErrorUnknownHCICommand: Int { get }
var kBluetoothHCIErrorNoConnection: Int { get }
var kBluetoothHCIErrorHardwareFailure: Int { get }
var kBluetoothHCIErrorPageTimeout: Int { get }
var kBluetoothHCIErrorAuthenticationFailure: Int { get }
var kBluetoothHCIErrorKeyMissing: Int { get }
var kBluetoothHCIErrorMemoryFull: Int { get }
var kBluetoothHCIErrorConnectionTimeout: Int { get }
var kBluetoothHCIErrorMaxNumberOfConnections: Int { get }
var kBluetoothHCIErrorMaxNumberOfSCOConnectionsToADevice: Int { get }
var kBluetoothHCIErrorACLConnectionAlreadyExists: Int { get }
var kBluetoothHCIErrorCommandDisallowed: Int { get }
var kBluetoothHCIErrorHostRejectedLimitedResources: Int { get }
var kBluetoothHCIErrorHostRejectedSecurityReasons: Int { get }
var kBluetoothHCIErrorHostRejectedRemoteDeviceIsPersonal: Int { get }
var kBluetoothHCIErrorHostTimeout: Int { get }
var kBluetoothHCIErrorUnsupportedFeatureOrParameterValue: Int { get }
var kBluetoothHCIErrorInvalidHCICommandParameters: Int { get }
var kBluetoothHCIErrorOtherEndTerminatedConnectionUserEnded: Int { get }
var kBluetoothHCIErrorOtherEndTerminatedConnectionLowResources: Int { get }
var kBluetoothHCIErrorOtherEndTerminatedConnectionAboutToPowerOff: Int { get }
var kBluetoothHCIErrorConnectionTerminatedByLocalHost: Int { get }
var kBluetoothHCIErrorRepeatedAttempts: Int { get }
var kBluetoothHCIErrorPairingNotAllowed: Int { get }
var kBluetoothHCIErrorUnknownLMPPDU: Int { get }
var kBluetoothHCIErrorUnsupportedRemoteFeature: Int { get }
var kBluetoothHCIErrorSCOOffsetRejected: Int { get }
var kBluetoothHCIErrorSCOIntervalRejected: Int { get }
var kBluetoothHCIErrorSCOAirModeRejected: Int { get }
var kBluetoothHCIErrorInvalidLMPParameters: Int { get }
var kBluetoothHCIErrorUnspecifiedError: Int { get }
var kBluetoothHCIErrorUnsupportedLMPParameterValue: Int { get }
var kBluetoothHCIErrorRoleChangeNotAllowed: Int { get }
var kBluetoothHCIErrorLMPResponseTimeout: Int { get }
var kBluetoothHCIErrorLMPErrorTransactionCollision: Int { get }
var kBluetoothHCIErrorLMPPDUNotAllowed: Int { get }
var kBluetoothHCIErrorEncryptionModeNotAcceptable: Int { get }
var kBluetoothHCIErrorUnitKeyUsed: Int { get }
var kBluetoothHCIErrorQoSNotSupported: Int { get }
var kBluetoothHCIErrorInstantPassed: Int { get }
var kBluetoothHCIErrorPairingWithUnitKeyNotSupported: Int { get }
var kBluetoothHCIErrorHostRejectedUnacceptableDeviceAddress: Int { get }
var kBluetoothHCIErrorDifferentTransactionCollision: Int { get }
var kBluetoothHCIErrorQoSUnacceptableParameter: Int { get }
var kBluetoothHCIErrorQoSRejected: Int { get }
var kBluetoothHCIErrorChannelClassificationNotSupported: Int { get }
var kBluetoothHCIErrorInsufficientSecurity: Int { get }
var kBluetoothHCIErrorParameterOutOfMandatoryRange: Int { get }
var kBluetoothHCIErrorRoleSwitchPending: Int { get }
var kBluetoothHCIErrorReservedSlotViolation: Int { get }
var kBluetoothHCIErrorRoleSwitchFailed: Int { get }
var kBluetoothHCIErrorExtendedInquiryResponseTooLarge: Int { get }
var kBluetoothHCIErrorSecureSimplePairingNotSupportedByHost: Int { get }
var kBluetoothHCIErrorHostBusyPairing: Int { get }
var kBluetoothHCIErrorConnectionRejectedDueToNoSuitableChannelFound: Int { get }
var kBluetoothHCIErrorControllerBusy: Int { get }
var kBluetoothHCIErrorUnacceptableConnectionInterval: Int { get }
var kBluetoothHCIErrorDirectedAdvertisingTimeout: Int { get }
var kBluetoothHCIErrorConnectionTerminatedDueToMICFailure: Int { get }
var kBluetoothHCIErrorConnectionFailedToBeEstablished: Int { get }
var kBluetoothHCIErrorMACConnectionFailed: Int { get }
var kBluetoothHCIErrorMax: Int { get }
struct BluetoothHCIPowerState : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothHCIPowerStateON: BluetoothHCIPowerState { get }
var kBluetoothHCIPowerStateOFF: BluetoothHCIPowerState { get }
var kBluetoothHCIPowerStateUnintialized: BluetoothHCIPowerState { get }
var kBluetoothHCIErrorPowerIsOFF: Int { get }
var kBluetoothHCITransportUSBClassCode: Int { get }
var kBluetoothHCITransportUSBSubClassCode: Int { get }
var kBluetoothHCITransportUSBProtocolCode: Int { get }
var kBluetoothL2CAPTCIEventIDReserved: Int { get }
var kBluetoothL2CAPTCIEventIDL2CA_ConnectInd: Int { get }
var kBluetoothL2CAPTCIEventIDL2CA_ConfigInd: Int { get }
var kBluetoothL2CAPTCIEventIDL2CA_DisconnectInd: Int { get }
var kBluetoothL2CAPTCIEventIDL2CA_QoSViolationInd: Int { get }
var kBluetoothL2CAPTCIEventIDL2CA_TimeOutInd: Int { get }
var kBluetoothL2CAPTCICommandReserved: Int { get }
var kBluetoothL2CAPTCICommandL2CA_ConnectReq: Int { get }
var kBluetoothL2CAPTCICommandL2CA_DisconnectReq: Int { get }
var kBluetoothL2CAPTCICommandL2CA_ConfigReq: Int { get }
var kBluetoothL2CAPTCICommandL2CA_DisableCLT: Int { get }
var kBluetoothL2CAPTCICommandL2CA_EnableCLT: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GroupCreate: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GroupClose: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GroupAddMember: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GroupRemoveMember: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GroupMembership: Int { get }
var kBluetoothL2CAPTCICommandL2CA_WriteData: Int { get }
var kBluetoothL2CAPTCICommandL2CA_ReadData: Int { get }
var kBluetoothL2CAPTCICommandL2CA_Ping: Int { get }
var kBluetoothL2CAPTCICommandL2CA_GetInfo: Int { get }
var kBluetoothL2CAPTCICommandL2CA_Reserved1: Int { get }
var kBluetoothL2CAPTCICommandL2CA_Reserved2: Int { get }
var kBluetoothL2CAPTCICommandL2CA_ConnectResp: Int { get }
var kBluetoothL2CAPTCICommandL2CA_DisconnectResp: Int { get }
var kBluetoothL2CAPTCICommandL2CA_ConfigResp: Int { get }
var kMaxChannelIDPerSide: Int32 { get }
typealias BluetoothRFCOMMChannelID = UInt8
typealias BluetoothRFCOMMMTU = UInt16
struct BluetoothRFCOMMParityType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kBluetoothRFCOMMParityTypeNoParity: BluetoothRFCOMMParityType { get }
var kBluetoothRFCOMMParityTypeOddParity: BluetoothRFCOMMParityType { get }
var kBluetoothRFCOMMParityTypeEvenParity: BluetoothRFCOMMParityType { get }
var kBluetoothRFCOMMParityTypeMaxParity: BluetoothRFCOMMParityType { get }
struct BluetoothRFCOMMLineStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothRFCOMMLineStatusNoError: BluetoothRFCOMMLineStatus { get }
var BluetoothRFCOMMLineStatusOverrunError: BluetoothRFCOMMLineStatus { get }
var BluetoothRFCOMMLineStatusParityError: BluetoothRFCOMMLineStatus { get }
var BluetoothRFCOMMLineStatusFramingError: BluetoothRFCOMMLineStatus { get }
typealias BluetoothSDPPDUID = UInt8
var kBluetoothSDPPDUIDReserved: Int { get }
var kBluetoothSDPPDUIDErrorResponse: Int { get }
var kBluetoothSDPPDUIDServiceSearchRequest: Int { get }
var kBluetoothSDPPDUIDServiceSearchResponse: Int { get }
var kBluetoothSDPPDUIDServiceAttributeRequest: Int { get }
var kBluetoothSDPPDUIDServiceAttributeResponse: Int { get }
var kBluetoothSDPPDUIDServiceSearchAttributeRequest: Int { get }
var kBluetoothSDPPDUIDServiceSearchAttributeResponse: Int { get }
typealias BluetoothSDPErrorCode = UInt16
var kBluetoothSDPErrorCodeSuccess: Int { get }
var kBluetoothSDPErrorCodeReserved: Int { get }
var kBluetoothSDPErrorCodeInvalidSDPVersion: Int { get }
var kBluetoothSDPErrorCodeInvalidServiceRecordHandle: Int { get }
var kBluetoothSDPErrorCodeInvalidRequestSyntax: Int { get }
var kBluetoothSDPErrorCodeInvalidPDUSize: Int { get }
var kBluetoothSDPErrorCodeInvalidContinuationState: Int { get }
var kBluetoothSDPErrorCodeInsufficientResources: Int { get }
var kBluetoothSDPErrorCodeReservedStart: Int { get }
var kBluetoothSDPErrorCodeReservedEnd: Int { get }
typealias BluetoothSDPTransactionID = UInt16
typealias BluetoothSDPServiceRecordHandle = UInt32
var kBluetoothSDPDataElementTypeNil: Int { get }
var kBluetoothSDPDataElementTypeUnsignedInt: Int { get }
var kBluetoothSDPDataElementTypeSignedInt: Int { get }
var kBluetoothSDPDataElementTypeUUID: Int { get }
var kBluetoothSDPDataElementTypeString: Int { get }
var kBluetoothSDPDataElementTypeBoolean: Int { get }
var kBluetoothSDPDataElementTypeDataElementSequence: Int { get }
var kBluetoothSDPDataElementTypeDataElementAlternative: Int { get }
var kBluetoothSDPDataElementTypeURL: Int { get }
var kBluetoothSDPDataElementTypeReservedStart: Int { get }
var kBluetoothSDPDataElementTypeReservedEnd: Int { get }
typealias BluetoothSDPUUID16 = UInt16
typealias BluetoothSDPUUID32 = UInt32
typealias BluetoothSDPDataElementTypeDescriptor = UInt8
typealias BluetoothSDPDataElementSizeDescriptor = UInt8
typealias BluetoothSDPServiceAttributeID = UInt16
struct BluetoothLEScanType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEScanTypePassive: BluetoothLEScanType { get }
var BluetoothLEScanTypeActive: BluetoothLEScanType { get }
struct BluetoothLEAddressType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEAddressTypePublic: BluetoothLEAddressType { get }
var BluetoothLEAddressTypeRandom: BluetoothLEAddressType { get }
struct BluetoothLEScanFilter : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEScanFilterNone: BluetoothLEScanFilter { get }
var BluetoothLEScanFilterWhitelist: BluetoothLEScanFilter { get }
struct BluetoothLEScan : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEScanDisable: BluetoothLEScan { get }
var BluetoothLEScanEnable: BluetoothLEScan { get }
struct BluetoothLEConnectionInterval : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEConnectionIntervalMin: BluetoothLEConnectionInterval { get }
var BluetoothLEConnectionIntervalMax: BluetoothLEConnectionInterval { get }
struct BluetoothLEScanDuplicateFilter : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEScanDuplicateFilterDisable: BluetoothLEScanDuplicateFilter { get }
var BluetoothLEScanDuplicateFilterEnable: BluetoothLEScanDuplicateFilter { get }
struct BluetoothLEAdvertisingType : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var BluetoothLEAdvertisingTypeConnectableUndirected: BluetoothLEAdvertisingType { get }
var BluetoothLEAdvertisingTypeConnectableDirected: BluetoothLEAdvertisingType { get }
var BluetoothLEAdvertisingTypeDiscoverableUndirected: BluetoothLEAdvertisingType { get }
var BluetoothLEAdvertisingTypeNonConnectableUndirected: BluetoothLEAdvertisingType { get }
var BluetoothLEAdvertisingTypeScanResponse: BluetoothLEAdvertisingType { get }
