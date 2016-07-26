
class SecKeychain {
}
class SecKeychainItem {
}
class SecKeychainSearch {
}
typealias SecKeychainAttrType = OSType
struct SecKeychainAttribute {
  var tag: SecKeychainAttrType
  var length: UInt32
  var data: UnsafeMutablePointer<Void>
}
typealias SecKeychainAttributePtr = UnsafeMutablePointer<SecKeychainAttribute>
struct SecKeychainAttributeList {
  var count: UInt32
  var attr: UnsafeMutablePointer<SecKeychainAttribute>
}
typealias SecKeychainStatus = UInt32
class SecTrustedApplication {
}
class SecPolicy {
}
class SecCertificate {
}
class SecAccess {
}
class SecIdentity {
}
class SecKey {
}
class SecACL {
}
class SecAccessControl {
}
class SecPassword {
}
struct SecKeychainAttributeInfo {
  var count: UInt32
  var tag: UnsafeMutablePointer<UInt32>
  var format: UnsafeMutablePointer<UInt32>
}
@available(OSX 10.3, *)
@discardableResult
func SecCopyErrorMessageString(_ status: OSStatus, _ reserved: UnsafeMutablePointer<Void>?) -> CFString?
var errSecSuccess: OSStatus { get }
var errSecUnimplemented: OSStatus { get }
var errSecDskFull: OSStatus { get }
var errSecIO: OSStatus { get }
var errSecParam: OSStatus { get }
var errSecWrPerm: OSStatus { get }
var errSecAllocate: OSStatus { get }
var errSecUserCanceled: OSStatus { get }
var errSecBadReq: OSStatus { get }
var errSecInternalComponent: OSStatus { get }
var errSecCoreFoundationUnknown: OSStatus { get }
var errSecNotAvailable: OSStatus { get }
var errSecReadOnly: OSStatus { get }
var errSecAuthFailed: OSStatus { get }
var errSecNoSuchKeychain: OSStatus { get }
var errSecInvalidKeychain: OSStatus { get }
var errSecDuplicateKeychain: OSStatus { get }
var errSecDuplicateCallback: OSStatus { get }
var errSecInvalidCallback: OSStatus { get }
var errSecDuplicateItem: OSStatus { get }
var errSecItemNotFound: OSStatus { get }
var errSecBufferTooSmall: OSStatus { get }
var errSecDataTooLarge: OSStatus { get }
var errSecNoSuchAttr: OSStatus { get }
var errSecInvalidItemRef: OSStatus { get }
var errSecInvalidSearchRef: OSStatus { get }
var errSecNoSuchClass: OSStatus { get }
var errSecNoDefaultKeychain: OSStatus { get }
var errSecInteractionNotAllowed: OSStatus { get }
var errSecReadOnlyAttr: OSStatus { get }
var errSecWrongSecVersion: OSStatus { get }
var errSecKeySizeNotAllowed: OSStatus { get }
var errSecNoStorageModule: OSStatus { get }
var errSecNoCertificateModule: OSStatus { get }
var errSecNoPolicyModule: OSStatus { get }
var errSecInteractionRequired: OSStatus { get }
var errSecDataNotAvailable: OSStatus { get }
var errSecDataNotModifiable: OSStatus { get }
var errSecCreateChainFailed: OSStatus { get }
var errSecInvalidPrefsDomain: OSStatus { get }
var errSecInDarkWake: OSStatus { get }
var errSecACLNotSimple: OSStatus { get }
var errSecPolicyNotFound: OSStatus { get }
var errSecInvalidTrustSetting: OSStatus { get }
var errSecNoAccessForItem: OSStatus { get }
var errSecInvalidOwnerEdit: OSStatus { get }
var errSecTrustNotAvailable: OSStatus { get }
var errSecUnsupportedFormat: OSStatus { get }
var errSecUnknownFormat: OSStatus { get }
var errSecKeyIsSensitive: OSStatus { get }
var errSecMultiplePrivKeys: OSStatus { get }
var errSecPassphraseRequired: OSStatus { get }
var errSecInvalidPasswordRef: OSStatus { get }
var errSecInvalidTrustSettings: OSStatus { get }
var errSecNoTrustSettings: OSStatus { get }
var errSecPkcs12VerifyFailure: OSStatus { get }
var errSecNotSigner: OSStatus { get }
var errSecDecode: OSStatus { get }
var errSecServiceNotAvailable: OSStatus { get }
var errSecInsufficientClientID: OSStatus { get }
var errSecDeviceReset: OSStatus { get }
var errSecDeviceFailed: OSStatus { get }
var errSecAppleAddAppACLSubject: OSStatus { get }
var errSecApplePublicKeyIncomplete: OSStatus { get }
var errSecAppleSignatureMismatch: OSStatus { get }
var errSecAppleInvalidKeyStartDate: OSStatus { get }
var errSecAppleInvalidKeyEndDate: OSStatus { get }
var errSecConversionError: OSStatus { get }
var errSecAppleSSLv2Rollback: OSStatus { get }
var errSecDiskFull: OSStatus { get }
var errSecQuotaExceeded: OSStatus { get }
var errSecFileTooBig: OSStatus { get }
var errSecInvalidDatabaseBlob: OSStatus { get }
var errSecInvalidKeyBlob: OSStatus { get }
var errSecIncompatibleDatabaseBlob: OSStatus { get }
var errSecIncompatibleKeyBlob: OSStatus { get }
var errSecHostNameMismatch: OSStatus { get }
var errSecUnknownCriticalExtensionFlag: OSStatus { get }
var errSecNoBasicConstraints: OSStatus { get }
var errSecNoBasicConstraintsCA: OSStatus { get }
var errSecInvalidAuthorityKeyID: OSStatus { get }
var errSecInvalidSubjectKeyID: OSStatus { get }
var errSecInvalidKeyUsageForPolicy: OSStatus { get }
var errSecInvalidExtendedKeyUsage: OSStatus { get }
var errSecInvalidIDLinkage: OSStatus { get }
var errSecPathLengthConstraintExceeded: OSStatus { get }
var errSecInvalidRoot: OSStatus { get }
var errSecCRLExpired: OSStatus { get }
var errSecCRLNotValidYet: OSStatus { get }
var errSecCRLNotFound: OSStatus { get }
var errSecCRLServerDown: OSStatus { get }
var errSecCRLBadURI: OSStatus { get }
var errSecUnknownCertExtension: OSStatus { get }
var errSecUnknownCRLExtension: OSStatus { get }
var errSecCRLNotTrusted: OSStatus { get }
var errSecCRLPolicyFailed: OSStatus { get }
var errSecIDPFailure: OSStatus { get }
var errSecSMIMEEmailAddressesNotFound: OSStatus { get }
var errSecSMIMEBadExtendedKeyUsage: OSStatus { get }
var errSecSMIMEBadKeyUsage: OSStatus { get }
var errSecSMIMEKeyUsageNotCritical: OSStatus { get }
var errSecSMIMENoEmailAddress: OSStatus { get }
var errSecSMIMESubjAltNameNotCritical: OSStatus { get }
var errSecSSLBadExtendedKeyUsage: OSStatus { get }
var errSecOCSPBadResponse: OSStatus { get }
var errSecOCSPBadRequest: OSStatus { get }
var errSecOCSPUnavailable: OSStatus { get }
var errSecOCSPStatusUnrecognized: OSStatus { get }
var errSecEndOfData: OSStatus { get }
var errSecIncompleteCertRevocationCheck: OSStatus { get }
var errSecNetworkFailure: OSStatus { get }
var errSecOCSPNotTrustedToAnchor: OSStatus { get }
var errSecRecordModified: OSStatus { get }
var errSecOCSPSignatureError: OSStatus { get }
var errSecOCSPNoSigner: OSStatus { get }
var errSecOCSPResponderMalformedReq: OSStatus { get }
var errSecOCSPResponderInternalError: OSStatus { get }
var errSecOCSPResponderTryLater: OSStatus { get }
var errSecOCSPResponderSignatureRequired: OSStatus { get }
var errSecOCSPResponderUnauthorized: OSStatus { get }
var errSecOCSPResponseNonceMismatch: OSStatus { get }
var errSecCodeSigningBadCertChainLength: OSStatus { get }
var errSecCodeSigningNoBasicConstraints: OSStatus { get }
var errSecCodeSigningBadPathLengthConstraint: OSStatus { get }
var errSecCodeSigningNoExtendedKeyUsage: OSStatus { get }
var errSecCodeSigningDevelopment: OSStatus { get }
var errSecResourceSignBadCertChainLength: OSStatus { get }
var errSecResourceSignBadExtKeyUsage: OSStatus { get }
var errSecTrustSettingDeny: OSStatus { get }
var errSecInvalidSubjectName: OSStatus { get }
var errSecUnknownQualifiedCertStatement: OSStatus { get }
var errSecMobileMeRequestQueued: OSStatus { get }
var errSecMobileMeRequestRedirected: OSStatus { get }
var errSecMobileMeServerError: OSStatus { get }
var errSecMobileMeServerNotAvailable: OSStatus { get }
var errSecMobileMeServerAlreadyExists: OSStatus { get }
var errSecMobileMeServerServiceErr: OSStatus { get }
var errSecMobileMeRequestAlreadyPending: OSStatus { get }
var errSecMobileMeNoRequestPending: OSStatus { get }
var errSecMobileMeCSRVerifyFailure: OSStatus { get }
var errSecMobileMeFailedConsistencyCheck: OSStatus { get }
var errSecNotInitialized: OSStatus { get }
var errSecInvalidHandleUsage: OSStatus { get }
var errSecPVCReferentNotFound: OSStatus { get }
var errSecFunctionIntegrityFail: OSStatus { get }
var errSecInternalError: OSStatus { get }
var errSecMemoryError: OSStatus { get }
var errSecInvalidData: OSStatus { get }
var errSecMDSError: OSStatus { get }
var errSecInvalidPointer: OSStatus { get }
var errSecSelfCheckFailed: OSStatus { get }
var errSecFunctionFailed: OSStatus { get }
var errSecModuleManifestVerifyFailed: OSStatus { get }
var errSecInvalidGUID: OSStatus { get }
var errSecInvalidHandle: OSStatus { get }
var errSecInvalidDBList: OSStatus { get }
var errSecInvalidPassthroughID: OSStatus { get }
var errSecInvalidNetworkAddress: OSStatus { get }
var errSecCRLAlreadySigned: OSStatus { get }
var errSecInvalidNumberOfFields: OSStatus { get }
var errSecVerificationFailure: OSStatus { get }
var errSecUnknownTag: OSStatus { get }
var errSecInvalidSignature: OSStatus { get }
var errSecInvalidName: OSStatus { get }
var errSecInvalidCertificateRef: OSStatus { get }
var errSecInvalidCertificateGroup: OSStatus { get }
var errSecTagNotFound: OSStatus { get }
var errSecInvalidQuery: OSStatus { get }
var errSecInvalidValue: OSStatus { get }
var errSecCallbackFailed: OSStatus { get }
var errSecACLDeleteFailed: OSStatus { get }
var errSecACLReplaceFailed: OSStatus { get }
var errSecACLAddFailed: OSStatus { get }
var errSecACLChangeFailed: OSStatus { get }
var errSecInvalidAccessCredentials: OSStatus { get }
var errSecInvalidRecord: OSStatus { get }
var errSecInvalidACL: OSStatus { get }
var errSecInvalidSampleValue: OSStatus { get }
var errSecIncompatibleVersion: OSStatus { get }
var errSecPrivilegeNotGranted: OSStatus { get }
var errSecInvalidScope: OSStatus { get }
var errSecPVCAlreadyConfigured: OSStatus { get }
var errSecInvalidPVC: OSStatus { get }
var errSecEMMLoadFailed: OSStatus { get }
var errSecEMMUnloadFailed: OSStatus { get }
var errSecAddinLoadFailed: OSStatus { get }
var errSecInvalidKeyRef: OSStatus { get }
var errSecInvalidKeyHierarchy: OSStatus { get }
var errSecAddinUnloadFailed: OSStatus { get }
var errSecLibraryReferenceNotFound: OSStatus { get }
var errSecInvalidAddinFunctionTable: OSStatus { get }
var errSecInvalidServiceMask: OSStatus { get }
var errSecModuleNotLoaded: OSStatus { get }
var errSecInvalidSubServiceID: OSStatus { get }
var errSecAttributeNotInContext: OSStatus { get }
var errSecModuleManagerInitializeFailed: OSStatus { get }
var errSecModuleManagerNotFound: OSStatus { get }
var errSecEventNotificationCallbackNotFound: OSStatus { get }
var errSecInputLengthError: OSStatus { get }
var errSecOutputLengthError: OSStatus { get }
var errSecPrivilegeNotSupported: OSStatus { get }
var errSecDeviceError: OSStatus { get }
var errSecAttachHandleBusy: OSStatus { get }
var errSecNotLoggedIn: OSStatus { get }
var errSecAlgorithmMismatch: OSStatus { get }
var errSecKeyUsageIncorrect: OSStatus { get }
var errSecKeyBlobTypeIncorrect: OSStatus { get }
var errSecKeyHeaderInconsistent: OSStatus { get }
var errSecUnsupportedKeyFormat: OSStatus { get }
var errSecUnsupportedKeySize: OSStatus { get }
var errSecInvalidKeyUsageMask: OSStatus { get }
var errSecUnsupportedKeyUsageMask: OSStatus { get }
var errSecInvalidKeyAttributeMask: OSStatus { get }
var errSecUnsupportedKeyAttributeMask: OSStatus { get }
var errSecInvalidKeyLabel: OSStatus { get }
var errSecUnsupportedKeyLabel: OSStatus { get }
var errSecInvalidKeyFormat: OSStatus { get }
var errSecUnsupportedVectorOfBuffers: OSStatus { get }
var errSecInvalidInputVector: OSStatus { get }
var errSecInvalidOutputVector: OSStatus { get }
var errSecInvalidContext: OSStatus { get }
var errSecInvalidAlgorithm: OSStatus { get }
var errSecInvalidAttributeKey: OSStatus { get }
var errSecMissingAttributeKey: OSStatus { get }
var errSecInvalidAttributeInitVector: OSStatus { get }
var errSecMissingAttributeInitVector: OSStatus { get }
var errSecInvalidAttributeSalt: OSStatus { get }
var errSecMissingAttributeSalt: OSStatus { get }
var errSecInvalidAttributePadding: OSStatus { get }
var errSecMissingAttributePadding: OSStatus { get }
var errSecInvalidAttributeRandom: OSStatus { get }
var errSecMissingAttributeRandom: OSStatus { get }
var errSecInvalidAttributeSeed: OSStatus { get }
var errSecMissingAttributeSeed: OSStatus { get }
var errSecInvalidAttributePassphrase: OSStatus { get }
var errSecMissingAttributePassphrase: OSStatus { get }
var errSecInvalidAttributeKeyLength: OSStatus { get }
var errSecMissingAttributeKeyLength: OSStatus { get }
var errSecInvalidAttributeBlockSize: OSStatus { get }
var errSecMissingAttributeBlockSize: OSStatus { get }
var errSecInvalidAttributeOutputSize: OSStatus { get }
var errSecMissingAttributeOutputSize: OSStatus { get }
var errSecInvalidAttributeRounds: OSStatus { get }
var errSecMissingAttributeRounds: OSStatus { get }
var errSecInvalidAlgorithmParms: OSStatus { get }
var errSecMissingAlgorithmParms: OSStatus { get }
var errSecInvalidAttributeLabel: OSStatus { get }
var errSecMissingAttributeLabel: OSStatus { get }
var errSecInvalidAttributeKeyType: OSStatus { get }
var errSecMissingAttributeKeyType: OSStatus { get }
var errSecInvalidAttributeMode: OSStatus { get }
var errSecMissingAttributeMode: OSStatus { get }
var errSecInvalidAttributeEffectiveBits: OSStatus { get }
var errSecMissingAttributeEffectiveBits: OSStatus { get }
var errSecInvalidAttributeStartDate: OSStatus { get }
var errSecMissingAttributeStartDate: OSStatus { get }
var errSecInvalidAttributeEndDate: OSStatus { get }
var errSecMissingAttributeEndDate: OSStatus { get }
var errSecInvalidAttributeVersion: OSStatus { get }
var errSecMissingAttributeVersion: OSStatus { get }
var errSecInvalidAttributePrime: OSStatus { get }
var errSecMissingAttributePrime: OSStatus { get }
var errSecInvalidAttributeBase: OSStatus { get }
var errSecMissingAttributeBase: OSStatus { get }
var errSecInvalidAttributeSubprime: OSStatus { get }
var errSecMissingAttributeSubprime: OSStatus { get }
var errSecInvalidAttributeIterationCount: OSStatus { get }
var errSecMissingAttributeIterationCount: OSStatus { get }
var errSecInvalidAttributeDLDBHandle: OSStatus { get }
var errSecMissingAttributeDLDBHandle: OSStatus { get }
var errSecInvalidAttributeAccessCredentials: OSStatus { get }
var errSecMissingAttributeAccessCredentials: OSStatus { get }
var errSecInvalidAttributePublicKeyFormat: OSStatus { get }
var errSecMissingAttributePublicKeyFormat: OSStatus { get }
var errSecInvalidAttributePrivateKeyFormat: OSStatus { get }
var errSecMissingAttributePrivateKeyFormat: OSStatus { get }
var errSecInvalidAttributeSymmetricKeyFormat: OSStatus { get }
var errSecMissingAttributeSymmetricKeyFormat: OSStatus { get }
var errSecInvalidAttributeWrappedKeyFormat: OSStatus { get }
var errSecMissingAttributeWrappedKeyFormat: OSStatus { get }
var errSecStagedOperationInProgress: OSStatus { get }
var errSecStagedOperationNotStarted: OSStatus { get }
var errSecVerifyFailed: OSStatus { get }
var errSecQuerySizeUnknown: OSStatus { get }
var errSecBlockSizeMismatch: OSStatus { get }
var errSecPublicKeyInconsistent: OSStatus { get }
var errSecDeviceVerifyFailed: OSStatus { get }
var errSecInvalidLoginName: OSStatus { get }
var errSecAlreadyLoggedIn: OSStatus { get }
var errSecInvalidDigestAlgorithm: OSStatus { get }
var errSecInvalidCRLGroup: OSStatus { get }
var errSecCertificateCannotOperate: OSStatus { get }
var errSecCertificateExpired: OSStatus { get }
var errSecCertificateNotValidYet: OSStatus { get }
var errSecCertificateRevoked: OSStatus { get }
var errSecCertificateSuspended: OSStatus { get }
var errSecInsufficientCredentials: OSStatus { get }
var errSecInvalidAction: OSStatus { get }
var errSecInvalidAuthority: OSStatus { get }
var errSecVerifyActionFailed: OSStatus { get }
var errSecInvalidCertAuthority: OSStatus { get }
var errSecInvaldCRLAuthority: OSStatus { get }
var errSecInvalidCRLEncoding: OSStatus { get }
var errSecInvalidCRLType: OSStatus { get }
var errSecInvalidCRL: OSStatus { get }
var errSecInvalidFormType: OSStatus { get }
var errSecInvalidID: OSStatus { get }
var errSecInvalidIdentifier: OSStatus { get }
var errSecInvalidIndex: OSStatus { get }
var errSecInvalidPolicyIdentifiers: OSStatus { get }
var errSecInvalidTimeString: OSStatus { get }
var errSecInvalidReason: OSStatus { get }
var errSecInvalidRequestInputs: OSStatus { get }
var errSecInvalidResponseVector: OSStatus { get }
var errSecInvalidStopOnPolicy: OSStatus { get }
var errSecInvalidTuple: OSStatus { get }
var errSecMultipleValuesUnsupported: OSStatus { get }
var errSecNotTrusted: OSStatus { get }
var errSecNoDefaultAuthority: OSStatus { get }
var errSecRejectedForm: OSStatus { get }
var errSecRequestLost: OSStatus { get }
var errSecRequestRejected: OSStatus { get }
var errSecUnsupportedAddressType: OSStatus { get }
var errSecUnsupportedService: OSStatus { get }
var errSecInvalidTupleGroup: OSStatus { get }
var errSecInvalidBaseACLs: OSStatus { get }
var errSecInvalidTupleCredendtials: OSStatus { get }
var errSecInvalidEncoding: OSStatus { get }
var errSecInvalidValidityPeriod: OSStatus { get }
var errSecInvalidRequestor: OSStatus { get }
var errSecRequestDescriptor: OSStatus { get }
var errSecInvalidBundleInfo: OSStatus { get }
var errSecInvalidCRLIndex: OSStatus { get }
var errSecNoFieldValues: OSStatus { get }
var errSecUnsupportedFieldFormat: OSStatus { get }
var errSecUnsupportedIndexInfo: OSStatus { get }
var errSecUnsupportedLocality: OSStatus { get }
var errSecUnsupportedNumAttributes: OSStatus { get }
var errSecUnsupportedNumIndexes: OSStatus { get }
var errSecUnsupportedNumRecordTypes: OSStatus { get }
var errSecFieldSpecifiedMultiple: OSStatus { get }
var errSecIncompatibleFieldFormat: OSStatus { get }
var errSecInvalidParsingModule: OSStatus { get }
var errSecDatabaseLocked: OSStatus { get }
var errSecDatastoreIsOpen: OSStatus { get }
var errSecMissingValue: OSStatus { get }
var errSecUnsupportedQueryLimits: OSStatus { get }
var errSecUnsupportedNumSelectionPreds: OSStatus { get }
var errSecUnsupportedOperator: OSStatus { get }
var errSecInvalidDBLocation: OSStatus { get }
var errSecInvalidAccessRequest: OSStatus { get }
var errSecInvalidIndexInfo: OSStatus { get }
var errSecInvalidNewOwner: OSStatus { get }
var errSecInvalidModifyMode: OSStatus { get }
var errSecMissingRequiredExtension: OSStatus { get }
var errSecExtendedKeyUsageNotCritical: OSStatus { get }
var errSecTimestampMissing: OSStatus { get }
var errSecTimestampInvalid: OSStatus { get }
var errSecTimestampNotTrusted: OSStatus { get }
var errSecTimestampServiceNotAvailable: OSStatus { get }
var errSecTimestampBadAlg: OSStatus { get }
var errSecTimestampBadRequest: OSStatus { get }
var errSecTimestampBadDataFormat: OSStatus { get }
var errSecTimestampTimeNotAvailable: OSStatus { get }
var errSecTimestampUnacceptedPolicy: OSStatus { get }
var errSecTimestampUnacceptedExtension: OSStatus { get }
var errSecTimestampAddInfoNotAvailable: OSStatus { get }
var errSecTimestampSystemFailure: OSStatus { get }
var errSecSigningTimeMissing: OSStatus { get }
var errSecTimestampRejection: OSStatus { get }
var errSecTimestampWaiting: OSStatus { get }
var errSecTimestampRevocationWarning: OSStatus { get }
var errSecTimestampRevocationNotification: OSStatus { get }
