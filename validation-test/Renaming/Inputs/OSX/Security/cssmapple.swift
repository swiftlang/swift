
let gGuidCssm: cssm_guid
let gGuidAppleFileDL: cssm_guid
let gGuidAppleCSP: cssm_guid
let gGuidAppleCSPDL: cssm_guid
let gGuidAppleX509CL: cssm_guid
let gGuidAppleX509TP: cssm_guid
let gGuidAppleLDAPDL: cssm_guid
let gGuidAppleDotMacTP: cssm_guid
let gGuidAppleSdCSPDL: cssm_guid
let gGuidAppleDotMacDL: cssm_guid
var CSSM_WORDID_KEYCHAIN_PROMPT: Int { get }
var CSSM_WORDID_KEYCHAIN_LOCK: Int { get }
var CSSM_WORDID_KEYCHAIN_CHANGE_LOCK: Int { get }
var CSSM_WORDID_PROCESS: Int { get }
var CSSM_WORDID__RESERVED_1: Int { get }
var CSSM_WORDID_SYMMETRIC_KEY: Int { get }
var CSSM_WORDID_SYSTEM: Int { get }
var CSSM_WORDID_KEY: Int { get }
var CSSM_WORDID_PIN: Int { get }
var CSSM_WORDID_PREAUTH: Int { get }
var CSSM_WORDID_PREAUTH_SOURCE: Int { get }
var CSSM_WORDID_ASYMMETRIC_KEY: Int { get }
var CSSM_WORDID_PARTITION: Int { get }
var CSSM_WORDID__FIRST_UNUSED: Int { get }
var CSSM_ACL_SUBJECT_TYPE_KEYCHAIN_PROMPT: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PROCESS: Int { get }
var CSSM_ACL_SUBJECT_TYPE_CODE_SIGNATURE: Int { get }
var CSSM_ACL_SUBJECT_TYPE_COMMENT: Int { get }
var CSSM_ACL_SUBJECT_TYPE_SYMMETRIC_KEY: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PREAUTH: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PREAUTH_SOURCE: Int { get }
var CSSM_ACL_SUBJECT_TYPE_ASYMMETRIC_KEY: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PARTITION: Int { get }
var CSSM_SAMPLE_TYPE_KEYCHAIN_PROMPT: Int { get }
var CSSM_SAMPLE_TYPE_KEYCHAIN_LOCK: Int { get }
var CSSM_SAMPLE_TYPE_KEYCHAIN_CHANGE_LOCK: Int { get }
var CSSM_SAMPLE_TYPE_PROCESS: Int { get }
var CSSM_SAMPLE_TYPE_COMMENT: Int { get }
var CSSM_SAMPLE_TYPE_RETRY_ID: Int { get }
var CSSM_SAMPLE_TYPE_SYMMETRIC_KEY: Int { get }
var CSSM_SAMPLE_TYPE_PREAUTH: Int { get }
var CSSM_SAMPLE_TYPE_ASYMMETRIC_KEY: Int { get }
var CSSM_ACL_AUTHORIZATION_CHANGE_ACL: Int { get }
var CSSM_ACL_AUTHORIZATION_CHANGE_OWNER: Int { get }
var CSSM_ACL_AUTHORIZATION_PARTITION_ID: Int { get }
var CSSM_ACL_AUTHORIZATION_INTEGRITY: Int { get }
var CSSM_ACL_AUTHORIZATION_PREAUTH_BASE: Int { get }
var CSSM_ACL_AUTHORIZATION_PREAUTH_END: Int { get }
var CSSM_ACL_CODE_SIGNATURE_INVALID: Int { get }
var CSSM_ACL_CODE_SIGNATURE_OSX: Int { get }
var CSSM_ACL_MATCH_UID: Int { get }
var CSSM_ACL_MATCH_GID: Int { get }
var CSSM_ACL_MATCH_HONOR_ROOT: Int { get }
var CSSM_ACL_MATCH_BITS: Int { get }
var CSSM_ACL_PROCESS_SELECTOR_CURRENT_VERSION: Int { get }
struct cssm_acl_process_subject_selector {
  var version: uint16
  var mask: uint16
  var uid: uint32
  var gid: uint32
  init()
  init(version version: uint16, mask mask: uint16, uid uid: uint32, gid gid: uint32)
}
typealias CSSM_ACL_PROCESS_SUBJECT_SELECTOR = cssm_acl_process_subject_selector
var CSSM_ACL_KEYCHAIN_PROMPT_CURRENT_VERSION: Int { get }
var CSSM_ACL_KEYCHAIN_PROMPT_REQUIRE_PASSPHRASE: Int { get }
var CSSM_ACL_KEYCHAIN_PROMPT_UNSIGNED: Int { get }
var CSSM_ACL_KEYCHAIN_PROMPT_UNSIGNED_ACT: Int { get }
var CSSM_ACL_KEYCHAIN_PROMPT_INVALID: Int { get }
var CSSM_ACL_KEYCHAIN_PROMPT_INVALID_ACT: Int { get }
struct cssm_acl_keychain_prompt_selector {
  var version: uint16
  var flags: uint16
  init()
  init(version version: uint16, flags flags: uint16)
}
typealias CSSM_ACL_KEYCHAIN_PROMPT_SELECTOR = cssm_acl_keychain_prompt_selector
typealias CSSM_ACL_PREAUTH_TRACKING_STATE = uint32
var CSSM_ACL_PREAUTH_TRACKING_COUNT_MASK: UInt32 { get }
var CSSM_ACL_PREAUTH_TRACKING_BLOCKED: UInt32 { get }
var CSSM_ACL_PREAUTH_TRACKING_UNKNOWN: UInt32 { get }
var CSSM_ACL_PREAUTH_TRACKING_AUTHORIZED: UInt32 { get }
var CSSM_DB_ACCESS_RESET: Int { get }
var CSSM_ALGID_APPLE_YARROW: UInt32 { get }
var CSSM_ALGID_AES: UInt32 { get }
var CSSM_ALGID_FEE: UInt32 { get }
var CSSM_ALGID_FEE_MD5: UInt32 { get }
var CSSM_ALGID_FEE_SHA1: UInt32 { get }
var CSSM_ALGID_FEED: UInt32 { get }
var CSSM_ALGID_FEEDEXP: UInt32 { get }
var CSSM_ALGID_ASC: UInt32 { get }
var CSSM_ALGID_SHA1HMAC_LEGACY: UInt32 { get }
var CSSM_ALGID_KEYCHAIN_KEY: UInt32 { get }
var CSSM_ALGID_PKCS12_PBE_ENCR: UInt32 { get }
var CSSM_ALGID_PKCS12_PBE_MAC: UInt32 { get }
var CSSM_ALGID_SECURE_PASSPHRASE: UInt32 { get }
var CSSM_ALGID_PBE_OPENSSL_MD5: UInt32 { get }
var CSSM_ALGID_SHA256: UInt32 { get }
var CSSM_ALGID_SHA384: UInt32 { get }
var CSSM_ALGID_SHA512: UInt32 { get }
var CSSM_ALGID_ENTROPY_DEFAULT: UInt32 { get }
var CSSM_ALGID_SHA224: UInt32 { get }
var CSSM_ALGID_SHA224WithRSA: UInt32 { get }
var CSSM_ALGID_SHA256WithRSA: UInt32 { get }
var CSSM_ALGID_SHA384WithRSA: UInt32 { get }
var CSSM_ALGID_SHA512WithRSA: UInt32 { get }
var CSSM_ALGID_OPENSSH1: UInt32 { get }
var CSSM_ALGID_SHA224WithECDSA: UInt32 { get }
var CSSM_ALGID_SHA256WithECDSA: UInt32 { get }
var CSSM_ALGID_SHA384WithECDSA: UInt32 { get }
var CSSM_ALGID_SHA512WithECDSA: UInt32 { get }
var CSSM_ALGID_ECDSA_SPECIFIED: UInt32 { get }
var CSSM_ALGID_ECDH_X963_KDF: UInt32 { get }
var CSSM_ALGID__FIRST_UNUSED: UInt32 { get }
var CSSM_PADDING_APPLE_SSLv2: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_VENDOR_DEFINED: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_X509: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_OPENSSH: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_OPENSSL: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_OPENSSH2: UInt32 { get }
var CSSM_CUSTOM_COMMON_ERROR_EXTENT: Int { get }
var CSSM_ERRCODE_NO_USER_INTERACTION: Int { get }
var CSSM_ERRCODE_USER_CANCELED: Int { get }
var CSSM_ERRCODE_SERVICE_NOT_AVAILABLE: Int { get }
var CSSM_ERRCODE_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSM_ERRCODE_DEVICE_RESET: Int { get }
var CSSM_ERRCODE_DEVICE_FAILED: Int { get }
var CSSM_ERRCODE_IN_DARK_WAKE: Int { get }
var CSSMERR_CSSM_NO_USER_INTERACTION: Int { get }
var CSSMERR_AC_NO_USER_INTERACTION: Int { get }
var CSSMERR_CSP_NO_USER_INTERACTION: Int { get }
var CSSMERR_CL_NO_USER_INTERACTION: Int { get }
var CSSMERR_DL_NO_USER_INTERACTION: Int { get }
var CSSMERR_TP_NO_USER_INTERACTION: Int { get }
var CSSMERR_CSSM_USER_CANCELED: Int { get }
var CSSMERR_AC_USER_CANCELED: Int { get }
var CSSMERR_CSP_USER_CANCELED: Int { get }
var CSSMERR_CL_USER_CANCELED: Int { get }
var CSSMERR_DL_USER_CANCELED: Int { get }
var CSSMERR_TP_USER_CANCELED: Int { get }
var CSSMERR_CSSM_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_AC_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_CSP_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_CL_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_DL_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_TP_SERVICE_NOT_AVAILABLE: Int { get }
var CSSMERR_CSSM_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_AC_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_CSP_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_CL_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_DL_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_TP_INSUFFICIENT_CLIENT_IDENTIFICATION: Int { get }
var CSSMERR_CSSM_DEVICE_RESET: Int { get }
var CSSMERR_AC_DEVICE_RESET: Int { get }
var CSSMERR_CSP_DEVICE_RESET: Int { get }
var CSSMERR_CL_DEVICE_RESET: Int { get }
var CSSMERR_DL_DEVICE_RESET: Int { get }
var CSSMERR_TP_DEVICE_RESET: Int { get }
var CSSMERR_CSSM_DEVICE_FAILED: Int { get }
var CSSMERR_AC_DEVICE_FAILED: Int { get }
var CSSMERR_CSP_DEVICE_FAILED: Int { get }
var CSSMERR_CL_DEVICE_FAILED: Int { get }
var CSSMERR_DL_DEVICE_FAILED: Int { get }
var CSSMERR_TP_DEVICE_FAILED: Int { get }
var CSSMERR_CSSM_IN_DARK_WAKE: Int { get }
var CSSMERR_AC_IN_DARK_WAKE: Int { get }
var CSSMERR_CSP_IN_DARK_WAKE: Int { get }
var CSSMERR_CL_IN_DARK_WAKE: Int { get }
var CSSMERR_DL_IN_DARK_WAKE: Int { get }
var CSSMERR_TP_IN_DARK_WAKE: Int { get }
var CSSMERR_CSP_APPLE_ADD_APPLICATION_ACL_SUBJECT: Int { get }
var CSSMERR_CSP_APPLE_PUBLIC_KEY_INCOMPLETE: Int { get }
var CSSMERR_CSP_APPLE_SIGNATURE_MISMATCH: Int { get }
var CSSMERR_CSP_APPLE_INVALID_KEY_START_DATE: Int { get }
var CSSMERR_CSP_APPLE_INVALID_KEY_END_DATE: Int { get }
var CSSMERR_CSPDL_APPLE_DL_CONVERSION_ERROR: Int { get }
var CSSMERR_CSP_APPLE_SSLv2_ROLLBACK: Int { get }
var CSSM_DL_DB_RECORD_GENERIC_PASSWORD: UInt32 { get }
var CSSM_DL_DB_RECORD_INTERNET_PASSWORD: UInt32 { get }
var CSSM_DL_DB_RECORD_APPLESHARE_PASSWORD: UInt32 { get }
var CSSM_DL_DB_RECORD_X509_CERTIFICATE: UInt32 { get }
var CSSM_DL_DB_RECORD_USER_TRUST: UInt32 { get }
var CSSM_DL_DB_RECORD_X509_CRL: UInt32 { get }
var CSSM_DL_DB_RECORD_UNLOCK_REFERRAL: UInt32 { get }
var CSSM_DL_DB_RECORD_EXTENDED_ATTRIBUTE: UInt32 { get }
var CSSM_DL_DB_RECORD_METADATA: UInt32 { get }
var CSSM_APPLEFILEDL_TOGGLE_AUTOCOMMIT: Int { get }
var CSSM_APPLEFILEDL_COMMIT: Int { get }
var CSSM_APPLEFILEDL_ROLLBACK: Int { get }
var CSSM_APPLEFILEDL_TAKE_FILE_LOCK: Int { get }
var CSSM_APPLEFILEDL_MAKE_BACKUP: Int { get }
var CSSM_APPLE_UNLOCK_TYPE_KEY_DIRECT: Int { get }
var CSSM_APPLE_UNLOCK_TYPE_WRAPPED_PRIVATE: Int { get }
var CSSMERR_APPLEDL_INVALID_OPEN_PARAMETERS: Int { get }
var CSSMERR_APPLEDL_DISK_FULL: Int { get }
var CSSMERR_APPLEDL_QUOTA_EXCEEDED: Int { get }
var CSSMERR_APPLEDL_FILE_TOO_BIG: Int { get }
var CSSMERR_APPLEDL_INVALID_DATABASE_BLOB: Int { get }
var CSSMERR_APPLEDL_INVALID_KEY_BLOB: Int { get }
var CSSMERR_APPLEDL_INCOMPATIBLE_DATABASE_BLOB: Int { get }
var CSSMERR_APPLEDL_INCOMPATIBLE_KEY_BLOB: Int { get }
var CSSMERR_APPLETP_HOSTNAME_MISMATCH: Int { get }
var CSSMERR_APPLETP_UNKNOWN_CRITICAL_EXTEN: Int { get }
var CSSMERR_APPLETP_NO_BASIC_CONSTRAINTS: Int { get }
var CSSMERR_APPLETP_INVALID_CA: Int { get }
var CSSMERR_APPLETP_INVALID_AUTHORITY_ID: Int { get }
var CSSMERR_APPLETP_INVALID_SUBJECT_ID: Int { get }
var CSSMERR_APPLETP_INVALID_KEY_USAGE: Int { get }
var CSSMERR_APPLETP_INVALID_EXTENDED_KEY_USAGE: Int { get }
var CSSMERR_APPLETP_INVALID_ID_LINKAGE: Int { get }
var CSSMERR_APPLETP_PATH_LEN_CONSTRAINT: Int { get }
var CSSMERR_APPLETP_INVALID_ROOT: Int { get }
var CSSMERR_APPLETP_CRL_EXPIRED: Int { get }
var CSSMERR_APPLETP_CRL_NOT_VALID_YET: Int { get }
var CSSMERR_APPLETP_CRL_NOT_FOUND: Int { get }
var CSSMERR_APPLETP_CRL_SERVER_DOWN: Int { get }
var CSSMERR_APPLETP_CRL_BAD_URI: Int { get }
var CSSMERR_APPLETP_UNKNOWN_CERT_EXTEN: Int { get }
var CSSMERR_APPLETP_UNKNOWN_CRL_EXTEN: Int { get }
var CSSMERR_APPLETP_CRL_NOT_TRUSTED: Int { get }
var CSSMERR_APPLETP_CRL_INVALID_ANCHOR_CERT: Int { get }
var CSSMERR_APPLETP_CRL_POLICY_FAIL: Int { get }
var CSSMERR_APPLETP_IDP_FAIL: Int { get }
var CSSMERR_APPLETP_CERT_NOT_FOUND_FROM_ISSUER: Int { get }
var CSSMERR_APPLETP_BAD_CERT_FROM_ISSUER: Int { get }
var CSSMERR_APPLETP_SMIME_EMAIL_ADDRS_NOT_FOUND: Int { get }
var CSSMERR_APPLETP_SMIME_BAD_EXT_KEY_USE: Int { get }
var CSSMERR_APPLETP_SMIME_BAD_KEY_USE: Int { get }
var CSSMERR_APPLETP_SMIME_KEYUSAGE_NOT_CRITICAL: Int { get }
var CSSMERR_APPLETP_SMIME_NO_EMAIL_ADDRS: Int { get }
var CSSMERR_APPLETP_SMIME_SUBJ_ALT_NAME_NOT_CRIT: Int { get }
var CSSMERR_APPLETP_SSL_BAD_EXT_KEY_USE: Int { get }
var CSSMERR_APPLETP_OCSP_BAD_RESPONSE: Int { get }
var CSSMERR_APPLETP_OCSP_BAD_REQUEST: Int { get }
var CSSMERR_APPLETP_OCSP_UNAVAILABLE: Int { get }
var CSSMERR_APPLETP_OCSP_STATUS_UNRECOGNIZED: Int { get }
var CSSMERR_APPLETP_INCOMPLETE_REVOCATION_CHECK: Int { get }
var CSSMERR_APPLETP_NETWORK_FAILURE: Int { get }
var CSSMERR_APPLETP_OCSP_NOT_TRUSTED: Int { get }
var CSSMERR_APPLETP_OCSP_INVALID_ANCHOR_CERT: Int { get }
var CSSMERR_APPLETP_OCSP_SIG_ERROR: Int { get }
var CSSMERR_APPLETP_OCSP_NO_SIGNER: Int { get }
var CSSMERR_APPLETP_OCSP_RESP_MALFORMED_REQ: Int { get }
var CSSMERR_APPLETP_OCSP_RESP_INTERNAL_ERR: Int { get }
var CSSMERR_APPLETP_OCSP_RESP_TRY_LATER: Int { get }
var CSSMERR_APPLETP_OCSP_RESP_SIG_REQUIRED: Int { get }
var CSSMERR_APPLETP_OCSP_RESP_UNAUTHORIZED: Int { get }
var CSSMERR_APPLETP_OCSP_NONCE_MISMATCH: Int { get }
var CSSMERR_APPLETP_CS_BAD_CERT_CHAIN_LENGTH: Int { get }
var CSSMERR_APPLETP_CS_NO_BASIC_CONSTRAINTS: Int { get }
var CSSMERR_APPLETP_CS_BAD_PATH_LENGTH: Int { get }
var CSSMERR_APPLETP_CS_NO_EXTENDED_KEY_USAGE: Int { get }
var CSSMERR_APPLETP_CODE_SIGN_DEVELOPMENT: Int { get }
var CSSMERR_APPLETP_RS_BAD_CERT_CHAIN_LENGTH: Int { get }
var CSSMERR_APPLETP_RS_BAD_EXTENDED_KEY_USAGE: Int { get }
var CSSMERR_APPLETP_TRUST_SETTING_DENY: Int { get }
var CSSMERR_APPLETP_INVALID_EMPTY_SUBJECT: Int { get }
var CSSMERR_APPLETP_UNKNOWN_QUAL_CERT_STATEMENT: Int { get }
var CSSMERR_APPLETP_MISSING_REQUIRED_EXTENSION: Int { get }
var CSSMERR_APPLETP_EXT_KEYUSAGE_NOT_CRITICAL: Int { get }
var CSSMERR_APPLETP_IDENTIFIER_MISSING: Int { get }
var CSSMERR_APPLETP_CA_PIN_MISMATCH: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_QUEUED: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_REDIRECT: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_ERR: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_PARAM: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_AUTH: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_UNIMPL: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_NOT_AVAIL: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_ALREADY_EXIST: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_SERVER_SERVICE_ERROR: Int { get }
var CSSMERR_APPLE_DOTMAC_REQ_IS_PENDING: Int { get }
var CSSMERR_APPLE_DOTMAC_NO_REQ_PENDING: Int { get }
var CSSMERR_APPLE_DOTMAC_CSR_VERIFY_FAIL: Int { get }
var CSSMERR_APPLE_DOTMAC_FAILED_CONSISTENCY_CHECK: Int { get }
var CSSM_APPLEDL_OPEN_PARAMETERS_VERSION: Int { get }
struct cssm_appledl_open_parameters_mask : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCSSM_APPLEDL_MASK_MODE: cssm_appledl_open_parameters_mask { get }
struct cssm_appledl_open_parameters {
  var length: uint32
  var version: uint32
  var autoCommit: CSSM_BOOL
  var mask: uint32
  var mode: mode_t
  init()
  init(length length: uint32, version version: uint32, autoCommit autoCommit: CSSM_BOOL, mask mask: uint32, mode mode: mode_t)
}
typealias CSSM_APPLEDL_OPEN_PARAMETERS = cssm_appledl_open_parameters
typealias CSSM_APPLEDL_OPEN_PARAMETERS_PTR = UnsafeMutablePointer<cssm_appledl_open_parameters>
var CSSM_APPLECSPDL_DB_LOCK: Int { get }
var CSSM_APPLECSPDL_DB_UNLOCK: Int { get }
var CSSM_APPLECSPDL_DB_GET_SETTINGS: Int { get }
var CSSM_APPLECSPDL_DB_SET_SETTINGS: Int { get }
var CSSM_APPLECSPDL_DB_IS_LOCKED: Int { get }
var CSSM_APPLECSPDL_DB_CHANGE_PASSWORD: Int { get }
var CSSM_APPLECSPDL_DB_GET_HANDLE: Int { get }
var CSSM_APPLESCPDL_CSP_GET_KEYHANDLE: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_8: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_9: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_10: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_11: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_12: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_13: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_14: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_15: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_16: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_17: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_18: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_19: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_20: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_21: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_22: Int { get }
var CSSM_APPLE_PRIVATE_CSPDL_CODE_23: Int { get }
var CSSM_APPLECSP_KEYDIGEST: Int { get }
struct cssm_applecspdl_db_settings_parameters {
  var idleTimeout: uint32
  var lockOnSleep: uint8
  init()
  init(idleTimeout idleTimeout: uint32, lockOnSleep lockOnSleep: uint8)
}
typealias CSSM_APPLECSPDL_DB_SETTINGS_PARAMETERS = cssm_applecspdl_db_settings_parameters
typealias CSSM_APPLECSPDL_DB_SETTINGS_PARAMETERS_PTR = UnsafeMutablePointer<cssm_applecspdl_db_settings_parameters>
struct cssm_applecspdl_db_is_locked_parameters {
  var isLocked: uint8
  init()
  init(isLocked isLocked: uint8)
}
typealias CSSM_APPLECSPDL_DB_IS_LOCKED_PARAMETERS = cssm_applecspdl_db_is_locked_parameters
typealias CSSM_APPLECSPDL_DB_IS_LOCKED_PARAMETERS_PTR = UnsafeMutablePointer<cssm_applecspdl_db_is_locked_parameters>
struct cssm_applecspdl_db_change_password_parameters {
  var accessCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(accessCredentials accessCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_APPLECSPDL_DB_CHANGE_PASSWORD_PARAMETERS = cssm_applecspdl_db_change_password_parameters
typealias CSSM_APPLECSPDL_DB_CHANGE_PASSWORD_PARAMETERS_PTR = UnsafeMutablePointer<cssm_applecspdl_db_change_password_parameters>
var CSSM_KEYBLOB_WRAPPED_FORMAT_APPLE_CUSTOM: Int { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_OPENSSL: Int { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_OPENSSH1: Int { get }
var CSSM_ATTRIBUTE_VENDOR_DEFINED: Int { get }
var CSSM_ATTRIBUTE_PUBLIC_KEY: Int { get }
var CSSM_ATTRIBUTE_FEE_PRIME_TYPE: Int { get }
var CSSM_ATTRIBUTE_FEE_CURVE_TYPE: Int { get }
var CSSM_ATTRIBUTE_ASC_OPTIMIZATION: Int { get }
var CSSM_ATTRIBUTE_RSA_BLINDING: Int { get }
var CSSM_ATTRIBUTE_PARAM_KEY: Int { get }
var CSSM_ATTRIBUTE_PROMPT: Int { get }
var CSSM_ATTRIBUTE_ALERT_TITLE: Int { get }
var CSSM_ATTRIBUTE_VERIFY_PASSPHRASE: Int { get }
var CSSM_FEE_PRIME_TYPE_DEFAULT: Int { get }
var CSSM_FEE_PRIME_TYPE_MERSENNE: Int { get }
var CSSM_FEE_PRIME_TYPE_FEE: Int { get }
var CSSM_FEE_PRIME_TYPE_GENERAL: Int { get }
var CSSM_FEE_CURVE_TYPE_DEFAULT: Int { get }
var CSSM_FEE_CURVE_TYPE_MONTGOMERY: Int { get }
var CSSM_FEE_CURVE_TYPE_WEIERSTRASS: Int { get }
var CSSM_FEE_CURVE_TYPE_ANSI_X9_62: Int { get }
var CSSM_ASC_OPTIMIZE_DEFAULT: Int { get }
var CSSM_ASC_OPTIMIZE_SIZE: Int { get }
var CSSM_ASC_OPTIMIZE_SECURITY: Int { get }
var CSSM_ASC_OPTIMIZE_TIME: Int { get }
var CSSM_ASC_OPTIMIZE_TIME_SIZE: Int { get }
var CSSM_ASC_OPTIMIZE_ASCII: Int { get }
var CSSM_KEYATTR_PARTIAL: Int { get }
var CSSM_KEYATTR_PUBLIC_KEY_ENCRYPT: Int { get }
struct CSSM_APPLE_TP_NAME_OID {
  var string: UnsafePointer<Int8>!
  var oid: UnsafePointer<CSSM_OID>!
  init()
  init(string string: UnsafePointer<Int8>!, oid oid: UnsafePointer<CSSM_OID>!)
}
struct CSSM_APPLE_TP_CERT_REQUEST {
  var cspHand: CSSM_CSP_HANDLE
  var clHand: CSSM_CL_HANDLE
  var serialNumber: uint32
  var numSubjectNames: uint32
  var subjectNames: UnsafeMutablePointer<CSSM_APPLE_TP_NAME_OID>!
  var numIssuerNames: uint32
  var issuerNames: UnsafeMutablePointer<CSSM_APPLE_TP_NAME_OID>!
  var issuerNameX509: UnsafeMutablePointer<cssm_x509_name>!
  var certPublicKey: UnsafePointer<cssm_key>!
  var issuerPrivateKey: UnsafePointer<cssm_key>!
  var signatureAlg: CSSM_ALGORITHMS
  var signatureOid: CSSM_OID
  var notBefore: uint32
  var notAfter: uint32
  var numExtensions: uint32
  var extensions: UnsafeMutablePointer<__CE_DataAndType>!
  var challengeString: UnsafePointer<Int8>!
  init()
  init(cspHand cspHand: CSSM_CSP_HANDLE, clHand clHand: CSSM_CL_HANDLE, serialNumber serialNumber: uint32, numSubjectNames numSubjectNames: uint32, subjectNames subjectNames: UnsafeMutablePointer<CSSM_APPLE_TP_NAME_OID>!, numIssuerNames numIssuerNames: uint32, issuerNames issuerNames: UnsafeMutablePointer<CSSM_APPLE_TP_NAME_OID>!, issuerNameX509 issuerNameX509: UnsafeMutablePointer<cssm_x509_name>!, certPublicKey certPublicKey: UnsafePointer<cssm_key>!, issuerPrivateKey issuerPrivateKey: UnsafePointer<cssm_key>!, signatureAlg signatureAlg: CSSM_ALGORITHMS, signatureOid signatureOid: CSSM_OID, notBefore notBefore: uint32, notAfter notAfter: uint32, numExtensions numExtensions: uint32, extensions extensions: UnsafeMutablePointer<__CE_DataAndType>!, challengeString challengeString: UnsafePointer<Int8>!)
}
var CSSM_APPLE_TP_SSL_OPTS_VERSION: Int32 { get }
var CSSM_APPLE_TP_SSL_CLIENT: Int32 { get }
struct CSSM_APPLE_TP_SSL_OPTIONS {
  var Version: uint32
  var ServerNameLen: uint32
  var ServerName: UnsafePointer<Int8>!
  var Flags: uint32
  init()
  init(Version Version: uint32, ServerNameLen ServerNameLen: uint32, ServerName ServerName: UnsafePointer<Int8>!, Flags Flags: uint32)
}
var CSSM_APPLE_TP_CRL_OPTS_VERSION: Int32 { get }
typealias CSSM_APPLE_TP_CRL_OPT_FLAGS = uint32
var CSSM_TP_ACTION_REQUIRE_CRL_PER_CERT: Int { get }
var CSSM_TP_ACTION_FETCH_CRL_FROM_NET: Int { get }
var CSSM_TP_ACTION_CRL_SUFFICIENT: Int { get }
var CSSM_TP_ACTION_REQUIRE_CRL_IF_PRESENT: Int { get }
struct CSSM_APPLE_TP_CRL_OPTIONS {
  var Version: uint32
  var CrlFlags: CSSM_APPLE_TP_CRL_OPT_FLAGS
  var crlStore: UnsafeMutablePointer<cssm_dl_db_handle>!
  init()
  init(Version Version: uint32, CrlFlags CrlFlags: CSSM_APPLE_TP_CRL_OPT_FLAGS, crlStore crlStore: UnsafeMutablePointer<cssm_dl_db_handle>!)
}
var CSSM_APPLE_TP_SMIME_OPTS_VERSION: Int32 { get }
struct CSSM_APPLE_TP_SMIME_OPTIONS {
  var Version: uint32
  var IntendedUsage: uint16
  var SenderEmailLen: uint32
  var SenderEmail: UnsafePointer<Int8>!
  init()
  init(Version Version: uint32, IntendedUsage IntendedUsage: uint16, SenderEmailLen SenderEmailLen: uint32, SenderEmail SenderEmail: UnsafePointer<Int8>!)
}
typealias CSSM_APPLE_TP_ACTION_FLAGS = uint32
var CSSM_TP_ACTION_ALLOW_EXPIRED: Int { get }
var CSSM_TP_ACTION_LEAF_IS_CA: Int { get }
var CSSM_TP_ACTION_FETCH_CERT_FROM_NET: Int { get }
var CSSM_TP_ACTION_ALLOW_EXPIRED_ROOT: Int { get }
var CSSM_TP_ACTION_REQUIRE_REV_PER_CERT: Int { get }
var CSSM_TP_ACTION_TRUST_SETTINGS: Int { get }
var CSSM_TP_ACTION_IMPLICIT_ANCHORS: Int { get }
var CSSM_APPLE_TP_ACTION_VERSION: Int32 { get }
struct CSSM_APPLE_TP_ACTION_DATA {
  var Version: uint32
  var ActionFlags: CSSM_APPLE_TP_ACTION_FLAGS
  init()
  init(Version Version: uint32, ActionFlags ActionFlags: CSSM_APPLE_TP_ACTION_FLAGS)
}
typealias CSSM_TP_APPLE_CERT_STATUS = uint32
var CSSM_CERT_STATUS_EXPIRED: Int { get }
var CSSM_CERT_STATUS_NOT_VALID_YET: Int { get }
var CSSM_CERT_STATUS_IS_IN_INPUT_CERTS: Int { get }
var CSSM_CERT_STATUS_IS_IN_ANCHORS: Int { get }
var CSSM_CERT_STATUS_IS_ROOT: Int { get }
var CSSM_CERT_STATUS_IS_FROM_NET: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_FOUND_USER: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_FOUND_ADMIN: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_FOUND_SYSTEM: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_TRUST: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_DENY: Int { get }
var CSSM_CERT_STATUS_TRUST_SETTINGS_IGNORED_ERROR: Int { get }
struct CSSM_TP_APPLE_EVIDENCE_INFO {
  var StatusBits: CSSM_TP_APPLE_CERT_STATUS
  var NumStatusCodes: uint32
  var StatusCodes: UnsafeMutablePointer<CSSM_RETURN>!
  var Index: uint32
  var DlDbHandle: cssm_dl_db_handle
  var UniqueRecord: UnsafeMutablePointer<cssm_db_unique_record>!
  init()
  init(StatusBits StatusBits: CSSM_TP_APPLE_CERT_STATUS, NumStatusCodes NumStatusCodes: uint32, StatusCodes StatusCodes: UnsafeMutablePointer<CSSM_RETURN>!, Index Index: uint32, DlDbHandle DlDbHandle: cssm_dl_db_handle, UniqueRecord UniqueRecord: UnsafeMutablePointer<cssm_db_unique_record>!)
}
var CSSM_TP_APPLE_EVIDENCE_VERSION: Int32 { get }
struct CSSM_TP_APPLE_EVIDENCE_HEADER {
  var Version: uint32
  init()
  init(Version Version: uint32)
}
var CSSM_EVIDENCE_FORM_APPLE_CUSTOM: UInt32 { get }
var CSSM_EVIDENCE_FORM_APPLE_HEADER: UInt32 { get }
var CSSM_EVIDENCE_FORM_APPLE_CERTGROUP: UInt32 { get }
var CSSM_EVIDENCE_FORM_APPLE_CERT_INFO: UInt32 { get }
var CSSM_APPLEX509CL_OBTAIN_CSR: Int { get }
var CSSM_APPLEX509CL_VERIFY_CSR: Int { get }
struct CSSM_APPLE_CL_CSR_REQUEST {
  var subjectNameX509: UnsafeMutablePointer<cssm_x509_name>!
  var signatureAlg: CSSM_ALGORITHMS
  var signatureOid: CSSM_OID
  var cspHand: CSSM_CSP_HANDLE
  var subjectPublicKey: UnsafePointer<cssm_key>!
  var subjectPrivateKey: UnsafePointer<cssm_key>!
  var challengeString: UnsafePointer<Int8>!
  init()
  init(subjectNameX509 subjectNameX509: UnsafeMutablePointer<cssm_x509_name>!, signatureAlg signatureAlg: CSSM_ALGORITHMS, signatureOid signatureOid: CSSM_OID, cspHand cspHand: CSSM_CSP_HANDLE, subjectPublicKey subjectPublicKey: UnsafePointer<cssm_key>!, subjectPrivateKey subjectPrivateKey: UnsafePointer<cssm_key>!, challengeString challengeString: UnsafePointer<Int8>!)
}
var CSSM_APPLE_CRL_END_OF_TIME: String { get }
var kKeychainSuffix: String { get }
var kSystemKeychainName: String { get }
var kSystemKeychainDir: String { get }
var kSystemUnlockFile: String { get }
var CSSM_APPLE_ACL_TAG_PARTITION_ID: String { get }
var CSSM_APPLE_ACL_TAG_INTEGRITY: String { get }
func cssmPerror(_ how: UnsafePointer<Int8>!, _ error: CSSM_RETURN)
@discardableResult
func cssmOidToAlg(_ oid: UnsafePointer<CSSM_OID>!, _ alg: UnsafeMutablePointer<CSSM_ALGORITHMS>!) -> Bool
@discardableResult
func cssmAlgToOid(_ algId: CSSM_ALGORITHMS) -> UnsafePointer<CSSM_OID>!
var errSecErrnoBase: Int32 { get }
var errSecErrnoLimit: Int32 { get }
