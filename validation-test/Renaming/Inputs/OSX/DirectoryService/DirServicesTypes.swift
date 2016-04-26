
var __DS_MACTYPES__: Int32 { get }
struct tDirStatus : RawRepresentable, Equatable {
  init(_ rawValue: Int32)
  init(rawValue rawValue: Int32)
  var rawValue: Int32
}
var eDSNoErr: tDirStatus { get }
var eDSOpenFailed: tDirStatus { get }
var eDSCloseFailed: tDirStatus { get }
var eDSOpenNodeFailed: tDirStatus { get }
var eDSBadDirRefences: tDirStatus { get }
var eDSNullRecordReference: tDirStatus { get }
var eDSMaxSessionsOpen: tDirStatus { get }
var eDSCannotAccessSession: tDirStatus { get }
var eDSDirSrvcNotOpened: tDirStatus { get }
var eDSNodeNotFound: tDirStatus { get }
var eDSUnknownNodeName: tDirStatus { get }
var eDSRegisterCustomFailed: tDirStatus { get }
var eDSGetCustomFailed: tDirStatus { get }
var eDSUnRegisterFailed: tDirStatus { get }
var eDSLocalDSDaemonInUse: tDirStatus { get }
var eDSNormalDSDaemonInUse: tDirStatus { get }
var eDSAllocationFailed: tDirStatus { get }
var eDSDeAllocateFailed: tDirStatus { get }
var eDSCustomBlockFailed: tDirStatus { get }
var eDSCustomUnblockFailed: tDirStatus { get }
var eDSCustomYieldFailed: tDirStatus { get }
var eDSCorruptBuffer: tDirStatus { get }
var eDSInvalidIndex: tDirStatus { get }
var eDSIndexOutOfRange: tDirStatus { get }
var eDSIndexNotFound: tDirStatus { get }
var eDSCorruptRecEntryData: tDirStatus { get }
var eDSRefSpaceFull: tDirStatus { get }
var eDSRefTableAllocError: tDirStatus { get }
var eDSInvalidReference: tDirStatus { get }
var eDSInvalidRefType: tDirStatus { get }
var eDSInvalidDirRef: tDirStatus { get }
var eDSInvalidNodeRef: tDirStatus { get }
var eDSInvalidRecordRef: tDirStatus { get }
var eDSInvalidAttrListRef: tDirStatus { get }
var eDSInvalidAttrValueRef: tDirStatus { get }
var eDSInvalidContinueData: tDirStatus { get }
var eDSInvalidBuffFormat: tDirStatus { get }
var eDSInvalidPatternMatchType: tDirStatus { get }
var eDSRefTableError: tDirStatus { get }
var eDSRefTableNilError: tDirStatus { get }
var eDSRefTableIndexOutOfBoundsError: tDirStatus { get }
var eDSRefTableEntryNilError: tDirStatus { get }
var eDSRefTableCSBPAllocError: tDirStatus { get }
var eDSRefTableFWAllocError: tDirStatus { get }
var eDSAuthFailed: tDirStatus { get }
var eDSAuthMethodNotSupported: tDirStatus { get }
var eDSAuthResponseBufTooSmall: tDirStatus { get }
var eDSAuthParameterError: tDirStatus { get }
var eDSAuthInBuffFormatError: tDirStatus { get }
var eDSAuthNoSuchEntity: tDirStatus { get }
var eDSAuthBadPassword: tDirStatus { get }
var eDSAuthContinueDataBad: tDirStatus { get }
var eDSAuthUnknownUser: tDirStatus { get }
var eDSAuthInvalidUserName: tDirStatus { get }
var eDSAuthCannotRecoverPasswd: tDirStatus { get }
var eDSAuthFailedClearTextOnly: tDirStatus { get }
var eDSAuthNoAuthServerFound: tDirStatus { get }
var eDSAuthServerError: tDirStatus { get }
var eDSInvalidContext: tDirStatus { get }
var eDSBadContextData: tDirStatus { get }
var eDSPermissionError: tDirStatus { get }
var eDSReadOnly: tDirStatus { get }
var eDSInvalidDomain: tDirStatus { get }
var eNetInfoError: tDirStatus { get }
var eDSInvalidRecordType: tDirStatus { get }
var eDSInvalidAttributeType: tDirStatus { get }
var eDSInvalidRecordName: tDirStatus { get }
var eDSAttributeNotFound: tDirStatus { get }
var eDSRecordAlreadyExists: tDirStatus { get }
var eDSRecordNotFound: tDirStatus { get }
var eDSAttributeDoesNotExist: tDirStatus { get }
var eDSRecordTypeDisabled: tDirStatus { get }
var eDSNoStdMappingAvailable: tDirStatus { get }
var eDSInvalidNativeMapping: tDirStatus { get }
var eDSSchemaError: tDirStatus { get }
var eDSAttributeValueNotFound: tDirStatus { get }
var eDSVersionMismatch: tDirStatus { get }
var eDSPlugInConfigFileError: tDirStatus { get }
var eDSInvalidPlugInConfigData: tDirStatus { get }
var eDSAuthNewPasswordRequired: tDirStatus { get }
var eDSAuthPasswordExpired: tDirStatus { get }
var eDSAuthPasswordQualityCheckFailed: tDirStatus { get }
var eDSAuthAccountDisabled: tDirStatus { get }
var eDSAuthAccountExpired: tDirStatus { get }
var eDSAuthAccountInactive: tDirStatus { get }
var eDSAuthPasswordTooShort: tDirStatus { get }
var eDSAuthPasswordTooLong: tDirStatus { get }
var eDSAuthPasswordNeedsLetter: tDirStatus { get }
var eDSAuthPasswordNeedsDigit: tDirStatus { get }
var eDSAuthPasswordChangeTooSoon: tDirStatus { get }
var eDSAuthInvalidLogonHours: tDirStatus { get }
var eDSAuthInvalidComputer: tDirStatus { get }
var eDSAuthMasterUnreachable: tDirStatus { get }
var eDSNullParameter: tDirStatus { get }
var eDSNullDataBuff: tDirStatus { get }
var eDSNullNodeName: tDirStatus { get }
var eDSNullRecEntryPtr: tDirStatus { get }
var eDSNullRecName: tDirStatus { get }
var eDSNullRecNameList: tDirStatus { get }
var eDSNullRecType: tDirStatus { get }
var eDSNullRecTypeList: tDirStatus { get }
var eDSNullAttribute: tDirStatus { get }
var eDSNullAttributeAccess: tDirStatus { get }
var eDSNullAttributeValue: tDirStatus { get }
var eDSNullAttributeType: tDirStatus { get }
var eDSNullAttributeTypeList: tDirStatus { get }
var eDSNullAttributeControlPtr: tDirStatus { get }
var eDSNullAttributeRequestList: tDirStatus { get }
var eDSNullDataList: tDirStatus { get }
var eDSNullDirNodeTypeList: tDirStatus { get }
var eDSNullAutMethod: tDirStatus { get }
var eDSNullAuthStepData: tDirStatus { get }
var eDSNullAuthStepDataResp: tDirStatus { get }
var eDSNullNodeInfoTypeList: tDirStatus { get }
var eDSNullPatternMatch: tDirStatus { get }
var eDSNullNodeNamePattern: tDirStatus { get }
var eDSNullTargetArgument: tDirStatus { get }
var eDSEmptyParameter: tDirStatus { get }
var eDSEmptyBuffer: tDirStatus { get }
var eDSEmptyNodeName: tDirStatus { get }
var eDSEmptyRecordName: tDirStatus { get }
var eDSEmptyRecordNameList: tDirStatus { get }
var eDSEmptyRecordType: tDirStatus { get }
var eDSEmptyRecordTypeList: tDirStatus { get }
var eDSEmptyRecordEntry: tDirStatus { get }
var eDSEmptyPatternMatch: tDirStatus { get }
var eDSEmptyNodeNamePattern: tDirStatus { get }
var eDSEmptyAttribute: tDirStatus { get }
var eDSEmptyAttributeType: tDirStatus { get }
var eDSEmptyAttributeTypeList: tDirStatus { get }
var eDSEmptyAttributeValue: tDirStatus { get }
var eDSEmptyAttributeRequestList: tDirStatus { get }
var eDSEmptyDataList: tDirStatus { get }
var eDSEmptyNodeInfoTypeList: tDirStatus { get }
var eDSEmptyAuthMethod: tDirStatus { get }
var eDSEmptyAuthStepData: tDirStatus { get }
var eDSEmptyAuthStepDataResp: tDirStatus { get }
var eDSEmptyPattern2Match: tDirStatus { get }
var eDSBadDataNodeLength: tDirStatus { get }
var eDSBadDataNodeFormat: tDirStatus { get }
var eDSBadSourceDataNode: tDirStatus { get }
var eDSBadTargetDataNode: tDirStatus { get }
var eDSBufferTooSmall: tDirStatus { get }
var eDSUnknownMatchType: tDirStatus { get }
var eDSUnSupportedMatchType: tDirStatus { get }
var eDSInvalDataList: tDirStatus { get }
var eDSAttrListError: tDirStatus { get }
var eServerNotRunning: tDirStatus { get }
var eUnknownAPICall: tDirStatus { get }
var eUnknownServerError: tDirStatus { get }
var eUnknownPlugIn: tDirStatus { get }
var ePlugInDataError: tDirStatus { get }
var ePlugInNotFound: tDirStatus { get }
var ePlugInError: tDirStatus { get }
var ePlugInInitError: tDirStatus { get }
var ePlugInNotActive: tDirStatus { get }
var ePlugInFailedToInitialize: tDirStatus { get }
var ePlugInCallTimedOut: tDirStatus { get }
var eNoSearchNodesFound: tDirStatus { get }
var eSearchPathNotDefined: tDirStatus { get }
var eNotHandledByThisNode: tDirStatus { get }
var eIPCSendError: tDirStatus { get }
var eIPCReceiveError: tDirStatus { get }
var eServerReplyError: tDirStatus { get }
var eDSTCPSendError: tDirStatus { get }
var eDSTCPReceiveError: tDirStatus { get }
var eDSTCPVersionMismatch: tDirStatus { get }
var eDSIPUnreachable: tDirStatus { get }
var eDSUnknownHost: tDirStatus { get }
var ePluginHandlerNotLoaded: tDirStatus { get }
var eNoPluginsLoaded: tDirStatus { get }
var ePluginAlreadyLoaded: tDirStatus { get }
var ePluginVersionNotFound: tDirStatus { get }
var ePluginNameNotFound: tDirStatus { get }
var eNoPluginFactoriesFound: tDirStatus { get }
var ePluginConfigAvailNotFound: tDirStatus { get }
var ePluginConfigFileNotFound: tDirStatus { get }
var eCFMGetFileSysRepErr: tDirStatus { get }
var eCFPlugInGetBundleErr: tDirStatus { get }
var eCFBndleGetInfoDictErr: tDirStatus { get }
var eCFDictGetValueErr: tDirStatus { get }
var eDSServerTimeout: tDirStatus { get }
var eDSContinue: tDirStatus { get }
var eDSInvalidHandle: tDirStatus { get }
var eDSSendFailed: tDirStatus { get }
var eDSReceiveFailed: tDirStatus { get }
var eDSBadPacket: tDirStatus { get }
var eDSInvalidTag: tDirStatus { get }
var eDSInvalidSession: tDirStatus { get }
var eDSInvalidName: tDirStatus { get }
var eDSUserUnknown: tDirStatus { get }
var eDSUnrecoverablePassword: tDirStatus { get }
var eDSAuthenticationFailed: tDirStatus { get }
var eDSBogusServer: tDirStatus { get }
var eDSOperationFailed: tDirStatus { get }
var eDSNotAuthorized: tDirStatus { get }
var eDSNetInfoError: tDirStatus { get }
var eDSContactMaster: tDirStatus { get }
var eDSServiceUnavailable: tDirStatus { get }
var eDSInvalidFilePath: tDirStatus { get }
var eDSOperationTimeout: tDirStatus { get }
var eFWGetDirNodeNameErr1: tDirStatus { get }
var eFWGetDirNodeNameErr2: tDirStatus { get }
var eFWGetDirNodeNameErr3: tDirStatus { get }
var eFWGetDirNodeNameErr4: tDirStatus { get }
var eParameterSendError: tDirStatus { get }
var eParameterReceiveError: tDirStatus { get }
var eServerSendError: tDirStatus { get }
var eServerReceiveError: tDirStatus { get }
var eMemoryError: tDirStatus { get }
var eMemoryAllocError: tDirStatus { get }
var eServerError: tDirStatus { get }
var eParameterError: tDirStatus { get }
var eDataReceiveErr_NoDirRef: tDirStatus { get }
var eDataReceiveErr_NoRecRef: tDirStatus { get }
var eDataReceiveErr_NoAttrListRef: tDirStatus { get }
var eDataReceiveErr_NoAttrValueListRef: tDirStatus { get }
var eDataReceiveErr_NoAttrEntry: tDirStatus { get }
var eDataReceiveErr_NoAttrValueEntry: tDirStatus { get }
var eDataReceiveErr_NoNodeCount: tDirStatus { get }
var eDataReceiveErr_NoAttrCount: tDirStatus { get }
var eDataReceiveErr_NoRecEntry: tDirStatus { get }
var eDataReceiveErr_NoRecEntryCount: tDirStatus { get }
var eDataReceiveErr_NoRecMatchCount: tDirStatus { get }
var eDataReceiveErr_NoDataBuff: tDirStatus { get }
var eDataReceiveErr_NoContinueData: tDirStatus { get }
var eDataReceiveErr_NoNodeChangeToken: tDirStatus { get }
var eNoLongerSupported: tDirStatus { get }
var eUndefinedError: tDirStatus { get }
var eNotYetImplemented: tDirStatus { get }
var eDSLastValue: tDirStatus { get }
struct tDirPatternMatch : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var eDSNoMatch1: tDirPatternMatch { get }
var eDSAnyMatch: tDirPatternMatch { get }
var eDSBeginAppleReserve1: tDirPatternMatch { get }
var eDSEndAppleReserve1: tDirPatternMatch { get }
var eDSExact: tDirPatternMatch { get }
var eDSStartsWith: tDirPatternMatch { get }
var eDSEndsWith: tDirPatternMatch { get }
var eDSContains: tDirPatternMatch { get }
var eDSLessThan: tDirPatternMatch { get }
var eDSGreaterThan: tDirPatternMatch { get }
var eDSLessEqual: tDirPatternMatch { get }
var eDSGreaterEqual: tDirPatternMatch { get }
var eDSWildCardPattern: tDirPatternMatch { get }
var eDSRegularExpression: tDirPatternMatch { get }
var eDSCompoundExpression: tDirPatternMatch { get }
var eDSiExact: tDirPatternMatch { get }
var eDSiStartsWith: tDirPatternMatch { get }
var eDSiEndsWith: tDirPatternMatch { get }
var eDSiContains: tDirPatternMatch { get }
var eDSiLessThan: tDirPatternMatch { get }
var eDSiGreaterThan: tDirPatternMatch { get }
var eDSiLessEqual: tDirPatternMatch { get }
var eDSiGreaterEqual: tDirPatternMatch { get }
var eDSiWildCardPattern: tDirPatternMatch { get }
var eDSiRegularExpression: tDirPatternMatch { get }
var eDSiCompoundExpression: tDirPatternMatch { get }
var eDSLocalNodeNames: tDirPatternMatch { get }
var eDSSearchNodeName: tDirPatternMatch { get }
var eDSConfigNodeName: tDirPatternMatch { get }
var eDSLocalHostedNodes: tDirPatternMatch { get }
var eDSAuthenticationSearchNodeName: tDirPatternMatch { get }
var eDSContactsSearchNodeName: tDirPatternMatch { get }
var eDSNetworkSearchNodeName: tDirPatternMatch { get }
var eDSDefaultNetworkNodes: tDirPatternMatch { get }
var eDSCacheNodeName: tDirPatternMatch { get }
var dDSBeginPlugInCustom: tDirPatternMatch { get }
var eDSEndPlugInCustom: tDirPatternMatch { get }
var eDSBeginAppleReserve2: tDirPatternMatch { get }
var eDSEndAppleReserve2: tDirPatternMatch { get }
var eDSNoMatch2: tDirPatternMatch { get }
typealias tDirReference = UInt32
typealias tDirNodeReference = UInt32
typealias tClientData = UnsafeMutablePointer<Void>
typealias tBuffer = UnsafeMutablePointer<Void>
typealias tContextData = UInt32
struct tDataBuffer {
  var fBufferSize: UInt32
  var fBufferLength: UInt32
  var fBufferData: (Int8)
  init()
  init(fBufferSize fBufferSize: UInt32, fBufferLength fBufferLength: UInt32, fBufferData fBufferData: (Int8))
}
typealias tDataBufferPtr = UnsafeMutablePointer<tDataBuffer>
typealias tDataNode = tDataBuffer
typealias tDataNodePtr = UnsafeMutablePointer<tDataNode>
struct tDataList {
  var fDataNodeCount: UInt32
  var fDataListHead: tDataNodePtr!
  init()
  init(fDataNodeCount fDataNodeCount: UInt32, fDataListHead fDataListHead: tDataNodePtr!)
}
typealias tDataListPtr = UnsafeMutablePointer<tDataList>
struct tAccessControlEntry {
  var fGuestAccessFlags: UInt32
  var fDirMemberFlags: UInt32
  var fDirNodeMemberFlags: UInt32
  var fOwnerFlags: UInt32
  var fAdministratorFlags: UInt32
  init()
  init(fGuestAccessFlags fGuestAccessFlags: UInt32, fDirMemberFlags fDirMemberFlags: UInt32, fDirNodeMemberFlags fDirNodeMemberFlags: UInt32, fOwnerFlags fOwnerFlags: UInt32, fAdministratorFlags fAdministratorFlags: UInt32)
}
typealias tAccessControlEntryPtr = UnsafeMutablePointer<tAccessControlEntry>
struct tAttributeValueEntry {
  var fAttributeValueID: UInt32
  var fAttributeValueData: tDataNode
  init()
  init(fAttributeValueID fAttributeValueID: UInt32, fAttributeValueData fAttributeValueData: tDataNode)
}
typealias tAttributeValueEntryPtr = UnsafeMutablePointer<tAttributeValueEntry>
typealias tAttributeValueListRef = UInt32
struct tAttributeEntry {
  var fReserved1: UInt32
  var fReserved2: tAccessControlEntry
  var fAttributeValueCount: UInt32
  var fAttributeDataSize: UInt32
  var fAttributeValueMaxSize: UInt32
  var fAttributeSignature: tDataNode
  init()
  init(fReserved1 fReserved1: UInt32, fReserved2 fReserved2: tAccessControlEntry, fAttributeValueCount fAttributeValueCount: UInt32, fAttributeDataSize fAttributeDataSize: UInt32, fAttributeValueMaxSize fAttributeValueMaxSize: UInt32, fAttributeSignature fAttributeSignature: tDataNode)
}
typealias tAttributeEntryPtr = UnsafeMutablePointer<tAttributeEntry>
typealias tAttributeListRef = UInt32
struct tRecordEntry {
  var fReserved1: UInt32
  var fReserved2: tAccessControlEntry
  var fRecordAttributeCount: UInt32
  var fRecordNameAndType: tDataNode
  init()
  init(fReserved1 fReserved1: UInt32, fReserved2 fReserved2: tAccessControlEntry, fRecordAttributeCount fRecordAttributeCount: UInt32, fRecordNameAndType fRecordNameAndType: tDataNode)
}
typealias tRecordEntryPtr = UnsafeMutablePointer<tRecordEntry>
typealias tRecordReference = UInt32
typealias fpCustomAllocate = @convention(c) (tDirReference, tClientData!, UInt32, UnsafeMutablePointer<tBuffer?>!) -> tDirStatus
typealias fpCustomDeAllocate = @convention(c) (tDirReference, tClientData!, tBuffer!) -> tDirStatus
typealias fpCustomThreadBlock = @convention(c) (tDirReference, tClientData!) -> tDirStatus
typealias fpCustomThreadUnBlock = @convention(c) (tDirReference, tClientData!) -> tDirStatus
typealias fpCustomThreadYield = @convention(c) (tDirReference, tClientData!) -> tDirStatus
