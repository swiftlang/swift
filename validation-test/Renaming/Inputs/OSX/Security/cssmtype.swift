
typealias CSSM_HANDLE = CSSM_INTPTR
typealias CSSM_HANDLE_PTR = UnsafeMutablePointer<CSSM_INTPTR>
typealias CSSM_LONG_HANDLE = uint64
typealias CSSM_LONG_HANDLE_PTR = UnsafeMutablePointer<uint64>
typealias CSSM_MODULE_HANDLE = CSSM_HANDLE
typealias CSSM_MODULE_HANDLE_PTR = UnsafeMutablePointer<CSSM_HANDLE>
typealias CSSM_CC_HANDLE = CSSM_LONG_HANDLE
typealias CSSM_CSP_HANDLE = CSSM_MODULE_HANDLE
typealias CSSM_TP_HANDLE = CSSM_MODULE_HANDLE
typealias CSSM_AC_HANDLE = CSSM_MODULE_HANDLE
typealias CSSM_CL_HANDLE = CSSM_MODULE_HANDLE
typealias CSSM_DL_HANDLE = CSSM_MODULE_HANDLE
typealias CSSM_DB_HANDLE = CSSM_MODULE_HANDLE
var CSSM_INVALID_HANDLE: Int { get }
typealias CSSM_BOOL = sint32
var CSSM_FALSE: Int { get }
var CSSM_TRUE: Int { get }
typealias CSSM_RETURN = sint32
var CSSM_OK: Int { get }
var CSSM_MODULE_STRING_SIZE: Int { get }
typealias CSSM_STRING = (Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8, Int8)
struct cssm_data {
  var Length: CSSM_SIZE
  var Data: UnsafeMutablePointer<uint8>!
  init()
  init(Length Length: CSSM_SIZE, Data Data: UnsafeMutablePointer<uint8>!)
}
struct cssm_guid {
  var Data1: uint32
  var Data2: uint16
  var Data3: uint16
  var Data4: (uint8, uint8, uint8, uint8, uint8, uint8, uint8, uint8)
  init()
  init(Data1 Data1: uint32, Data2 Data2: uint16, Data3 Data3: uint16, Data4 Data4: (uint8, uint8, uint8, uint8, uint8, uint8, uint8, uint8))
}
typealias CSSM_BITMASK = uint32
typealias CSSM_KEY_HIERARCHY = CSSM_BITMASK
var CSSM_KEY_HIERARCHY_NONE: Int { get }
var CSSM_KEY_HIERARCHY_INTEG: Int { get }
var CSSM_KEY_HIERARCHY_EXPORT: Int { get }
typealias CSSM_PVC_MODE = CSSM_BITMASK
var CSSM_PVC_NONE: Int { get }
var CSSM_PVC_APP: Int { get }
var CSSM_PVC_SP: Int { get }
typealias CSSM_PRIVILEGE_SCOPE = uint32
var CSSM_PRIVILEGE_SCOPE_NONE: Int { get }
var CSSM_PRIVILEGE_SCOPE_PROCESS: Int { get }
var CSSM_PRIVILEGE_SCOPE_THREAD: Int { get }
struct cssm_version {
  var Major: uint32
  var Minor: uint32
  init()
  init(Major Major: uint32, Minor Minor: uint32)
}
typealias CSSM_SERVICE_MASK = uint32
var CSSM_SERVICE_CSSM: Int { get }
var CSSM_SERVICE_CSP: Int { get }
var CSSM_SERVICE_DL: Int { get }
var CSSM_SERVICE_CL: Int { get }
var CSSM_SERVICE_TP: Int { get }
var CSSM_SERVICE_AC: Int { get }
var CSSM_SERVICE_KR: Int { get }
typealias CSSM_SERVICE_TYPE = CSSM_SERVICE_MASK
struct cssm_subservice_uid {
  var Guid: cssm_guid
  var Version: cssm_version
  var SubserviceId: uint32
  var SubserviceType: CSSM_SERVICE_TYPE
  init()
  init(Guid Guid: cssm_guid, Version Version: cssm_version, SubserviceId SubserviceId: uint32, SubserviceType SubserviceType: CSSM_SERVICE_TYPE)
}
typealias CSSM_MODULE_EVENT = uint32
typealias CSSM_MODULE_EVENT_PTR = UnsafeMutablePointer<uint32>
var CSSM_NOTIFY_INSERT: Int { get }
var CSSM_NOTIFY_REMOVE: Int { get }
var CSSM_NOTIFY_FAULT: Int { get }
typealias CSSM_API_ModuleEventHandler = @convention(c) (UnsafePointer<cssm_guid>!, UnsafeMutablePointer<Void>!, uint32, CSSM_SERVICE_TYPE, CSSM_MODULE_EVENT) -> CSSM_RETURN
typealias CSSM_ATTACH_FLAGS = uint32
var CSSM_ATTACH_READ_ONLY: Int { get }
typealias CSSM_PRIVILEGE = uint64
typealias CSSM_USEE_TAG = CSSM_PRIVILEGE
var CSSM_USEE_LAST: Int { get }
var CSSM_USEE_NONE: Int { get }
var CSSM_USEE_DOMESTIC: Int { get }
var CSSM_USEE_FINANCIAL: Int { get }
var CSSM_USEE_KRLE: Int { get }
var CSSM_USEE_KRENT: Int { get }
var CSSM_USEE_SSL: Int { get }
var CSSM_USEE_AUTHENTICATION: Int { get }
var CSSM_USEE_KEYEXCH: Int { get }
var CSSM_USEE_MEDICAL: Int { get }
var CSSM_USEE_INSURANCE: Int { get }
var CSSM_USEE_WEAK: Int { get }
typealias CSSM_NET_ADDRESS_TYPE = uint32
var CSSM_ADDR_NONE: Int { get }
var CSSM_ADDR_CUSTOM: Int { get }
var CSSM_ADDR_URL: Int { get }
var CSSM_ADDR_SOCKADDR: Int { get }
var CSSM_ADDR_NAME: Int { get }
struct cssm_net_address {
  var AddressType: CSSM_NET_ADDRESS_TYPE
  var Address: cssm_data
  init()
  init(AddressType AddressType: CSSM_NET_ADDRESS_TYPE, Address Address: cssm_data)
}
typealias CSSM_NET_PROTOCOL = uint32
var CSSM_NET_PROTO_NONE: Int { get }
var CSSM_NET_PROTO_CUSTOM: Int { get }
var CSSM_NET_PROTO_UNSPECIFIED: Int { get }
var CSSM_NET_PROTO_LDAP: Int { get }
var CSSM_NET_PROTO_LDAPS: Int { get }
var CSSM_NET_PROTO_LDAPNS: Int { get }
var CSSM_NET_PROTO_X500DAP: Int { get }
var CSSM_NET_PROTO_FTP: Int { get }
var CSSM_NET_PROTO_FTPS: Int { get }
var CSSM_NET_PROTO_OCSP: Int { get }
var CSSM_NET_PROTO_CMP: Int { get }
var CSSM_NET_PROTO_CMPS: Int { get }
typealias CSSM_CALLBACK = @convention(c) (UnsafeMutablePointer<cssm_data>!, UnsafeMutablePointer<Void>!) -> CSSM_RETURN
struct cssm_crypto_data {
  var Param: cssm_data
  var Callback: CSSM_CALLBACK!
  var CallerCtx: UnsafeMutablePointer<Void>!
  init()
  init(Param Param: cssm_data, Callback Callback: CSSM_CALLBACK!, CallerCtx CallerCtx: UnsafeMutablePointer<Void>!)
}
typealias CSSM_WORDID_TYPE = sint32
var CSSM_WORDID__UNK_: Int { get }
var CSSM_WORDID__NLU_: Int { get }
var CSSM_WORDID__STAR_: Int { get }
var CSSM_WORDID_A: Int { get }
var CSSM_WORDID_ACL: Int { get }
var CSSM_WORDID_ALPHA: Int { get }
var CSSM_WORDID_B: Int { get }
var CSSM_WORDID_BER: Int { get }
var CSSM_WORDID_BINARY: Int { get }
var CSSM_WORDID_BIOMETRIC: Int { get }
var CSSM_WORDID_C: Int { get }
var CSSM_WORDID_CANCELED: Int { get }
var CSSM_WORDID_CERT: Int { get }
var CSSM_WORDID_COMMENT: Int { get }
var CSSM_WORDID_CRL: Int { get }
var CSSM_WORDID_CUSTOM: Int { get }
var CSSM_WORDID_D: Int { get }
var CSSM_WORDID_DATE: Int { get }
var CSSM_WORDID_DB_DELETE: Int { get }
var CSSM_WORDID_DB_EXEC_STORED_QUERY: Int { get }
var CSSM_WORDID_DB_INSERT: Int { get }
var CSSM_WORDID_DB_MODIFY: Int { get }
var CSSM_WORDID_DB_READ: Int { get }
var CSSM_WORDID_DBS_CREATE: Int { get }
var CSSM_WORDID_DBS_DELETE: Int { get }
var CSSM_WORDID_DECRYPT: Int { get }
var CSSM_WORDID_DELETE: Int { get }
var CSSM_WORDID_DELTA_CRL: Int { get }
var CSSM_WORDID_DER: Int { get }
var CSSM_WORDID_DERIVE: Int { get }
var CSSM_WORDID_DISPLAY: Int { get }
var CSSM_WORDID_DO: Int { get }
var CSSM_WORDID_DSA: Int { get }
var CSSM_WORDID_DSA_SHA1: Int { get }
var CSSM_WORDID_E: Int { get }
var CSSM_WORDID_ELGAMAL: Int { get }
var CSSM_WORDID_ENCRYPT: Int { get }
var CSSM_WORDID_ENTRY: Int { get }
var CSSM_WORDID_EXPORT_CLEAR: Int { get }
var CSSM_WORDID_EXPORT_WRAPPED: Int { get }
var CSSM_WORDID_G: Int { get }
var CSSM_WORDID_GE: Int { get }
var CSSM_WORDID_GENKEY: Int { get }
var CSSM_WORDID_HASH: Int { get }
var CSSM_WORDID_HASHED_PASSWORD: Int { get }
var CSSM_WORDID_HASHED_SUBJECT: Int { get }
var CSSM_WORDID_HAVAL: Int { get }
var CSSM_WORDID_IBCHASH: Int { get }
var CSSM_WORDID_IMPORT_CLEAR: Int { get }
var CSSM_WORDID_IMPORT_WRAPPED: Int { get }
var CSSM_WORDID_INTEL: Int { get }
var CSSM_WORDID_ISSUER: Int { get }
var CSSM_WORDID_ISSUER_INFO: Int { get }
var CSSM_WORDID_K_OF_N: Int { get }
var CSSM_WORDID_KEA: Int { get }
var CSSM_WORDID_KEYHOLDER: Int { get }
var CSSM_WORDID_L: Int { get }
var CSSM_WORDID_LE: Int { get }
var CSSM_WORDID_LOGIN: Int { get }
var CSSM_WORDID_LOGIN_NAME: Int { get }
var CSSM_WORDID_MAC: Int { get }
var CSSM_WORDID_MD2: Int { get }
var CSSM_WORDID_MD2WITHRSA: Int { get }
var CSSM_WORDID_MD4: Int { get }
var CSSM_WORDID_MD5: Int { get }
var CSSM_WORDID_MD5WITHRSA: Int { get }
var CSSM_WORDID_N: Int { get }
var CSSM_WORDID_NAME: Int { get }
var CSSM_WORDID_NDR: Int { get }
var CSSM_WORDID_NHASH: Int { get }
var CSSM_WORDID_NOT_AFTER: Int { get }
var CSSM_WORDID_NOT_BEFORE: Int { get }
var CSSM_WORDID_NULL: Int { get }
var CSSM_WORDID_NUMERIC: Int { get }
var CSSM_WORDID_OBJECT_HASH: Int { get }
var CSSM_WORDID_ONE_TIME: Int { get }
var CSSM_WORDID_ONLINE: Int { get }
var CSSM_WORDID_OWNER: Int { get }
var CSSM_WORDID_P: Int { get }
var CSSM_WORDID_PAM_NAME: Int { get }
var CSSM_WORDID_PASSWORD: Int { get }
var CSSM_WORDID_PGP: Int { get }
var CSSM_WORDID_PREFIX: Int { get }
var CSSM_WORDID_PRIVATE_KEY: Int { get }
var CSSM_WORDID_PROMPTED_BIOMETRIC: Int { get }
var CSSM_WORDID_PROMPTED_PASSWORD: Int { get }
var CSSM_WORDID_PROPAGATE: Int { get }
var CSSM_WORDID_PROTECTED_BIOMETRIC: Int { get }
var CSSM_WORDID_PROTECTED_PASSWORD: Int { get }
var CSSM_WORDID_PROTECTED_PIN: Int { get }
var CSSM_WORDID_PUBLIC_KEY: Int { get }
var CSSM_WORDID_PUBLIC_KEY_FROM_CERT: Int { get }
var CSSM_WORDID_Q: Int { get }
var CSSM_WORDID_RANGE: Int { get }
var CSSM_WORDID_REVAL: Int { get }
var CSSM_WORDID_RIPEMAC: Int { get }
var CSSM_WORDID_RIPEMD: Int { get }
var CSSM_WORDID_RIPEMD160: Int { get }
var CSSM_WORDID_RSA: Int { get }
var CSSM_WORDID_RSA_ISO9796: Int { get }
var CSSM_WORDID_RSA_PKCS: Int { get }
var CSSM_WORDID_RSA_PKCS_MD5: Int { get }
var CSSM_WORDID_RSA_PKCS_SHA1: Int { get }
var CSSM_WORDID_RSA_PKCS1: Int { get }
var CSSM_WORDID_RSA_PKCS1_MD5: Int { get }
var CSSM_WORDID_RSA_PKCS1_SHA1: Int { get }
var CSSM_WORDID_RSA_PKCS1_SIG: Int { get }
var CSSM_WORDID_RSA_RAW: Int { get }
var CSSM_WORDID_SDSIV1: Int { get }
var CSSM_WORDID_SEQUENCE: Int { get }
var CSSM_WORDID_SET: Int { get }
var CSSM_WORDID_SEXPR: Int { get }
var CSSM_WORDID_SHA1: Int { get }
var CSSM_WORDID_SHA1WITHDSA: Int { get }
var CSSM_WORDID_SHA1WITHECDSA: Int { get }
var CSSM_WORDID_SHA1WITHRSA: Int { get }
var CSSM_WORDID_SIGN: Int { get }
var CSSM_WORDID_SIGNATURE: Int { get }
var CSSM_WORDID_SIGNED_NONCE: Int { get }
var CSSM_WORDID_SIGNED_SECRET: Int { get }
var CSSM_WORDID_SPKI: Int { get }
var CSSM_WORDID_SUBJECT: Int { get }
var CSSM_WORDID_SUBJECT_INFO: Int { get }
var CSSM_WORDID_TAG: Int { get }
var CSSM_WORDID_THRESHOLD: Int { get }
var CSSM_WORDID_TIME: Int { get }
var CSSM_WORDID_URI: Int { get }
var CSSM_WORDID_VERSION: Int { get }
var CSSM_WORDID_X509_ATTRIBUTE: Int { get }
var CSSM_WORDID_X509V1: Int { get }
var CSSM_WORDID_X509V2: Int { get }
var CSSM_WORDID_X509V3: Int { get }
var CSSM_WORDID_X9_ATTRIBUTE: Int { get }
var CSSM_WORDID_VENDOR_START: Int { get }
var CSSM_WORDID_VENDOR_END: Int { get }
typealias CSSM_LIST_ELEMENT_TYPE = uint32
typealias CSSM_LIST_ELEMENT_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_LIST_ELEMENT_DATUM: Int { get }
var CSSM_LIST_ELEMENT_SUBLIST: Int { get }
var CSSM_LIST_ELEMENT_WORDID: Int { get }
typealias CSSM_LIST_TYPE = uint32
typealias CSSM_LIST_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_LIST_TYPE_UNKNOWN: Int { get }
var CSSM_LIST_TYPE_CUSTOM: Int { get }
var CSSM_LIST_TYPE_SEXPR: Int { get }
typealias CSSM_LIST_ELEMENT_PTR = UnsafeMutablePointer<cssm_list_element>
struct cssm_list {
  var ListType: CSSM_LIST_TYPE
  var Head: CSSM_LIST_ELEMENT_PTR!
  var Tail: CSSM_LIST_ELEMENT_PTR!
  init()
  init(ListType ListType: CSSM_LIST_TYPE, Head Head: CSSM_LIST_ELEMENT_PTR!, Tail Tail: CSSM_LIST_ELEMENT_PTR!)
}
struct cssm_list_element {
  struct __Unnamed_union_Element {
    var Sublist: cssm_list
    var Word: cssm_data
    init(Sublist Sublist: cssm_list)
    init(Word Word: cssm_data)
    init()
  }
  var NextElement: UnsafeMutablePointer<cssm_list_element>!
  var WordID: CSSM_WORDID_TYPE
  var ElementType: CSSM_LIST_ELEMENT_TYPE
  var Element: cssm_list_element.__Unnamed_union_Element
  init()
  init(NextElement NextElement: UnsafeMutablePointer<cssm_list_element>!, WordID WordID: CSSM_WORDID_TYPE, ElementType ElementType: CSSM_LIST_ELEMENT_TYPE, Element Element: cssm_list_element.__Unnamed_union_Element)
}
typealias CSSM_LIST_ELEMENT = cssm_list_element
struct cssm_tuplegroup {
  var NumberOfTuples: uint32
  var Tuples: UnsafeMutablePointer<CSSM_TUPLE>!
  init()
  init(NumberOfTuples NumberOfTuples: uint32, Tuples Tuples: UnsafeMutablePointer<CSSM_TUPLE>!)
}
typealias CSSM_SAMPLE_TYPE = CSSM_WORDID_TYPE
var CSSM_SAMPLE_TYPE_PASSWORD: Int { get }
var CSSM_SAMPLE_TYPE_HASHED_PASSWORD: Int { get }
var CSSM_SAMPLE_TYPE_PROTECTED_PASSWORD: Int { get }
var CSSM_SAMPLE_TYPE_PROMPTED_PASSWORD: Int { get }
var CSSM_SAMPLE_TYPE_SIGNED_NONCE: Int { get }
var CSSM_SAMPLE_TYPE_SIGNED_SECRET: Int { get }
var CSSM_SAMPLE_TYPE_BIOMETRIC: Int { get }
var CSSM_SAMPLE_TYPE_PROTECTED_BIOMETRIC: Int { get }
var CSSM_SAMPLE_TYPE_PROMPTED_BIOMETRIC: Int { get }
var CSSM_SAMPLE_TYPE_THRESHOLD: Int { get }
struct cssm_sample {
  var TypedSample: cssm_list
  var Verifier: UnsafePointer<cssm_subservice_uid>!
  init()
  init(TypedSample TypedSample: cssm_list, Verifier Verifier: UnsafePointer<cssm_subservice_uid>!)
}
struct cssm_samplegroup {
  var NumberOfSamples: uint32
  var Samples: UnsafePointer<cssm_sample>!
  init()
  init(NumberOfSamples NumberOfSamples: uint32, Samples Samples: UnsafePointer<cssm_sample>!)
}
typealias CSSM_MALLOC = @convention(c) (CSSM_SIZE, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!
typealias CSSM_FREE = @convention(c) (UnsafeMutablePointer<Void>!, UnsafeMutablePointer<Void>!) -> Void
typealias CSSM_REALLOC = @convention(c) (UnsafeMutablePointer<Void>!, CSSM_SIZE, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!
typealias CSSM_CALLOC = @convention(c) (uint32, CSSM_SIZE, UnsafeMutablePointer<Void>!) -> UnsafeMutablePointer<Void>!
struct cssm_memory_funcs {
  var malloc_func: CSSM_MALLOC!
  var free_func: CSSM_FREE!
  var realloc_func: CSSM_REALLOC!
  var calloc_func: CSSM_CALLOC!
  var AllocRef: UnsafeMutablePointer<Void>!
  init()
  init(malloc_func malloc_func: CSSM_MALLOC!, free_func free_func: CSSM_FREE!, realloc_func realloc_func: CSSM_REALLOC!, calloc_func calloc_func: CSSM_CALLOC!, AllocRef AllocRef: UnsafeMutablePointer<Void>!)
}
typealias CSSM_API_MEMORY_FUNCS = cssm_memory_funcs
typealias CSSM_API_MEMORY_FUNCS_PTR = UnsafeMutablePointer<CSSM_API_MEMORY_FUNCS>
typealias CSSM_CHALLENGE_CALLBACK = @convention(c) (UnsafePointer<cssm_list>!, UnsafeMutablePointer<cssm_samplegroup>!, UnsafeMutablePointer<Void>!, UnsafePointer<cssm_memory_funcs>!) -> CSSM_RETURN
typealias CSSM_CERT_TYPE = uint32
typealias CSSM_CERT_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_CERT_UNKNOWN: Int { get }
var CSSM_CERT_X_509v1: Int { get }
var CSSM_CERT_X_509v2: Int { get }
var CSSM_CERT_X_509v3: Int { get }
var CSSM_CERT_PGP: Int { get }
var CSSM_CERT_SPKI: Int { get }
var CSSM_CERT_SDSIv1: Int { get }
var CSSM_CERT_Intel: Int { get }
var CSSM_CERT_X_509_ATTRIBUTE: Int { get }
var CSSM_CERT_X9_ATTRIBUTE: Int { get }
var CSSM_CERT_TUPLE: Int { get }
var CSSM_CERT_ACL_ENTRY: Int { get }
var CSSM_CERT_MULTIPLE: Int { get }
var CSSM_CERT_LAST: Int { get }
var CSSM_CL_CUSTOM_CERT_TYPE: Int { get }
typealias CSSM_CERT_ENCODING = uint32
typealias CSSM_CERT_ENCODING_PTR = UnsafeMutablePointer<uint32>
var CSSM_CERT_ENCODING_UNKNOWN: Int { get }
var CSSM_CERT_ENCODING_CUSTOM: Int { get }
var CSSM_CERT_ENCODING_BER: Int { get }
var CSSM_CERT_ENCODING_DER: Int { get }
var CSSM_CERT_ENCODING_NDR: Int { get }
var CSSM_CERT_ENCODING_SEXPR: Int { get }
var CSSM_CERT_ENCODING_PGP: Int { get }
var CSSM_CERT_ENCODING_MULTIPLE: Int { get }
var CSSM_CERT_ENCODING_LAST: Int { get }
var CSSM_CL_CUSTOM_CERT_ENCODING: Int { get }
struct cssm_encoded_cert {
  var CertType: CSSM_CERT_TYPE
  var CertEncoding: CSSM_CERT_ENCODING
  var CertBlob: cssm_data
  init()
  init(CertType CertType: CSSM_CERT_TYPE, CertEncoding CertEncoding: CSSM_CERT_ENCODING, CertBlob CertBlob: cssm_data)
}
typealias CSSM_CERT_PARSE_FORMAT = uint32
typealias CSSM_CERT_PARSE_FORMAT_PTR = UnsafeMutablePointer<uint32>
var CSSM_CERT_PARSE_FORMAT_NONE: Int { get }
var CSSM_CERT_PARSE_FORMAT_CUSTOM: Int { get }
var CSSM_CERT_PARSE_FORMAT_SEXPR: Int { get }
var CSSM_CERT_PARSE_FORMAT_COMPLEX: Int { get }
var CSSM_CERT_PARSE_FORMAT_OID_NAMED: Int { get }
var CSSM_CERT_PARSE_FORMAT_TUPLE: Int { get }
var CSSM_CERT_PARSE_FORMAT_MULTIPLE: Int { get }
var CSSM_CERT_PARSE_FORMAT_LAST: Int { get }
var CSSM_CL_CUSTOM_CERT_PARSE_FORMAT: Int { get }
struct cssm_parsed_cert {
  var CertType: CSSM_CERT_TYPE
  var ParsedCertFormat: CSSM_CERT_PARSE_FORMAT
  var ParsedCert: UnsafeMutablePointer<Void>!
  init()
  init(CertType CertType: CSSM_CERT_TYPE, ParsedCertFormat ParsedCertFormat: CSSM_CERT_PARSE_FORMAT, ParsedCert ParsedCert: UnsafeMutablePointer<Void>!)
}
struct cssm_cert_pair {
  var EncodedCert: cssm_encoded_cert
  var ParsedCert: cssm_parsed_cert
  init()
  init(EncodedCert EncodedCert: cssm_encoded_cert, ParsedCert ParsedCert: cssm_parsed_cert)
}
typealias CSSM_CERTGROUP_TYPE = uint32
typealias CSSM_CERTGROUP_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_CERTGROUP_DATA: Int { get }
var CSSM_CERTGROUP_ENCODED_CERT: Int { get }
var CSSM_CERTGROUP_PARSED_CERT: Int { get }
var CSSM_CERTGROUP_CERT_PAIR: Int { get }
struct cssm_certgroup {
  struct __Unnamed_union_GroupList {
    var CertList: UnsafeMutablePointer<cssm_data>!
    var EncodedCertList: UnsafeMutablePointer<cssm_encoded_cert>!
    var ParsedCertList: UnsafeMutablePointer<cssm_parsed_cert>!
    var PairCertList: UnsafeMutablePointer<cssm_cert_pair>!
    init(CertList CertList: UnsafeMutablePointer<cssm_data>!)
    init(EncodedCertList EncodedCertList: UnsafeMutablePointer<cssm_encoded_cert>!)
    init(ParsedCertList ParsedCertList: UnsafeMutablePointer<cssm_parsed_cert>!)
    init(PairCertList PairCertList: UnsafeMutablePointer<cssm_cert_pair>!)
    init()
  }
  var CertType: CSSM_CERT_TYPE
  var CertEncoding: CSSM_CERT_ENCODING
  var NumCerts: uint32
  var GroupList: cssm_certgroup.__Unnamed_union_GroupList
  var CertGroupType: CSSM_CERTGROUP_TYPE
  var Reserved: UnsafeMutablePointer<Void>!
  init()
  init(CertType CertType: CSSM_CERT_TYPE, CertEncoding CertEncoding: CSSM_CERT_ENCODING, NumCerts NumCerts: uint32, GroupList GroupList: cssm_certgroup.__Unnamed_union_GroupList, CertGroupType CertGroupType: CSSM_CERTGROUP_TYPE, Reserved Reserved: UnsafeMutablePointer<Void>!)
}
typealias CSSM_CERTGROUP = cssm_certgroup
typealias CSSM_CERTGROUP_PTR = UnsafeMutablePointer<cssm_certgroup>
struct cssm_base_certs {
  var TPHandle: CSSM_TP_HANDLE
  var CLHandle: CSSM_CL_HANDLE
  var Certs: CSSM_CERTGROUP
  init()
  init(TPHandle TPHandle: CSSM_TP_HANDLE, CLHandle CLHandle: CSSM_CL_HANDLE, Certs Certs: CSSM_CERTGROUP)
}
struct cssm_access_credentials {
  var EntryTag: CSSM_STRING
  var BaseCerts: cssm_base_certs
  var Samples: cssm_samplegroup
  var Callback: CSSM_CHALLENGE_CALLBACK!
  var CallerCtx: UnsafeMutablePointer<Void>!
  init()
  init(EntryTag EntryTag: CSSM_STRING, BaseCerts BaseCerts: cssm_base_certs, Samples Samples: cssm_samplegroup, Callback Callback: CSSM_CHALLENGE_CALLBACK!, CallerCtx CallerCtx: UnsafeMutablePointer<Void>!)
}
typealias CSSM_ACL_SUBJECT_TYPE = sint32
var CSSM_ACL_SUBJECT_TYPE_ANY: Int { get }
var CSSM_ACL_SUBJECT_TYPE_THRESHOLD: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PASSWORD: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PROTECTED_PASSWORD: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PROMPTED_PASSWORD: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PUBLIC_KEY: Int { get }
var CSSM_ACL_SUBJECT_TYPE_HASHED_SUBJECT: Int { get }
var CSSM_ACL_SUBJECT_TYPE_BIOMETRIC: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PROTECTED_BIOMETRIC: Int { get }
var CSSM_ACL_SUBJECT_TYPE_PROMPTED_BIOMETRIC: Int { get }
var CSSM_ACL_SUBJECT_TYPE_LOGIN_NAME: Int { get }
var CSSM_ACL_SUBJECT_TYPE_EXT_PAM_NAME: Int { get }
typealias CSSM_ACL_AUTHORIZATION_TAG = sint32
var CSSM_ACL_AUTHORIZATION_TAG_VENDOR_DEFINED_START: Int { get }
var CSSM_ACL_AUTHORIZATION_ANY: Int { get }
var CSSM_ACL_AUTHORIZATION_LOGIN: Int { get }
var CSSM_ACL_AUTHORIZATION_GENKEY: Int { get }
var CSSM_ACL_AUTHORIZATION_DELETE: Int { get }
var CSSM_ACL_AUTHORIZATION_EXPORT_WRAPPED: Int { get }
var CSSM_ACL_AUTHORIZATION_EXPORT_CLEAR: Int { get }
var CSSM_ACL_AUTHORIZATION_IMPORT_WRAPPED: Int { get }
var CSSM_ACL_AUTHORIZATION_IMPORT_CLEAR: Int { get }
var CSSM_ACL_AUTHORIZATION_SIGN: Int { get }
var CSSM_ACL_AUTHORIZATION_ENCRYPT: Int { get }
var CSSM_ACL_AUTHORIZATION_DECRYPT: Int { get }
var CSSM_ACL_AUTHORIZATION_MAC: Int { get }
var CSSM_ACL_AUTHORIZATION_DERIVE: Int { get }
var CSSM_ACL_AUTHORIZATION_DBS_CREATE: Int { get }
var CSSM_ACL_AUTHORIZATION_DBS_DELETE: Int { get }
var CSSM_ACL_AUTHORIZATION_DB_READ: Int { get }
var CSSM_ACL_AUTHORIZATION_DB_INSERT: Int { get }
var CSSM_ACL_AUTHORIZATION_DB_MODIFY: Int { get }
var CSSM_ACL_AUTHORIZATION_DB_DELETE: Int { get }
struct cssm_authorizationgroup {
  var NumberOfAuthTags: uint32
  var AuthTags: UnsafeMutablePointer<CSSM_ACL_AUTHORIZATION_TAG>!
  init()
  init(NumberOfAuthTags NumberOfAuthTags: uint32, AuthTags AuthTags: UnsafeMutablePointer<CSSM_ACL_AUTHORIZATION_TAG>!)
}
struct cssm_acl_validity_period {
  var StartDate: cssm_data
  var EndDate: cssm_data
  init()
  init(StartDate StartDate: cssm_data, EndDate EndDate: cssm_data)
}
struct cssm_acl_entry_prototype {
  var TypedSubject: cssm_list
  var Delegate: CSSM_BOOL
  var Authorization: cssm_authorizationgroup
  var TimeRange: cssm_acl_validity_period
  var EntryTag: CSSM_STRING
  init()
  init(TypedSubject TypedSubject: cssm_list, Delegate Delegate: CSSM_BOOL, Authorization Authorization: cssm_authorizationgroup, TimeRange TimeRange: cssm_acl_validity_period, EntryTag EntryTag: CSSM_STRING)
}
struct cssm_acl_owner_prototype {
  var TypedSubject: cssm_list
  var Delegate: CSSM_BOOL
  init()
  init(TypedSubject TypedSubject: cssm_list, Delegate Delegate: CSSM_BOOL)
}
typealias CSSM_ACL_SUBJECT_CALLBACK = @convention(c) (UnsafePointer<cssm_list>!, UnsafeMutablePointer<cssm_list>!, UnsafeMutablePointer<Void>!, UnsafePointer<cssm_memory_funcs>!) -> CSSM_RETURN
struct cssm_acl_entry_input {
  var Prototype: cssm_acl_entry_prototype
  var Callback: CSSM_ACL_SUBJECT_CALLBACK!
  var CallerContext: UnsafeMutablePointer<Void>!
  init()
  init(Prototype Prototype: cssm_acl_entry_prototype, Callback Callback: CSSM_ACL_SUBJECT_CALLBACK!, CallerContext CallerContext: UnsafeMutablePointer<Void>!)
}
struct cssm_resource_control_context {
  var AccessCred: UnsafeMutablePointer<cssm_access_credentials>!
  var InitialAclEntry: cssm_acl_entry_input
  init()
  init(AccessCred AccessCred: UnsafeMutablePointer<cssm_access_credentials>!, InitialAclEntry InitialAclEntry: cssm_acl_entry_input)
}
typealias CSSM_ACL_HANDLE = CSSM_HANDLE
struct cssm_acl_entry_info {
  var EntryPublicInfo: cssm_acl_entry_prototype
  var EntryHandle: CSSM_ACL_HANDLE
  init()
  init(EntryPublicInfo EntryPublicInfo: cssm_acl_entry_prototype, EntryHandle EntryHandle: CSSM_ACL_HANDLE)
}
typealias CSSM_ACL_EDIT_MODE = uint32
var CSSM_ACL_EDIT_MODE_ADD: Int { get }
var CSSM_ACL_EDIT_MODE_DELETE: Int { get }
var CSSM_ACL_EDIT_MODE_REPLACE: Int { get }
struct cssm_acl_edit {
  var EditMode: CSSM_ACL_EDIT_MODE
  var OldEntryHandle: CSSM_ACL_HANDLE
  var NewEntry: UnsafePointer<cssm_acl_entry_input>!
  init()
  init(EditMode EditMode: CSSM_ACL_EDIT_MODE, OldEntryHandle OldEntryHandle: CSSM_ACL_HANDLE, NewEntry NewEntry: UnsafePointer<cssm_acl_entry_input>!)
}
typealias CSSM_PROC_ADDR = @convention(c) () -> Void
typealias CSSM_PROC_ADDR_PTR = UnsafeMutablePointer<CSSM_PROC_ADDR?>
struct cssm_func_name_addr {
  var Name: CSSM_STRING
  var Address: CSSM_PROC_ADDR!
  init()
  init(Name Name: CSSM_STRING, Address Address: CSSM_PROC_ADDR!)
}
struct cssm_date {
  var Year: (uint8, uint8, uint8, uint8)
  var Month: (uint8, uint8)
  var Day: (uint8, uint8)
  init()
  init(Year Year: (uint8, uint8, uint8, uint8), Month Month: (uint8, uint8), Day Day: (uint8, uint8))
}
struct cssm_range {
  var Min: uint32
  var Max: uint32
  init()
  init(Min Min: uint32, Max Max: uint32)
}
struct cssm_query_size_data {
  var SizeInputBlock: uint32
  var SizeOutputBlock: uint32
  init()
  init(SizeInputBlock SizeInputBlock: uint32, SizeOutputBlock SizeOutputBlock: uint32)
}
typealias CSSM_HEADERVERSION = uint32
var CSSM_KEYHEADER_VERSION: Int { get }
struct cssm_key_size {
  var LogicalKeySizeInBits: uint32
  var EffectiveKeySizeInBits: uint32
  init()
  init(LogicalKeySizeInBits LogicalKeySizeInBits: uint32, EffectiveKeySizeInBits EffectiveKeySizeInBits: uint32)
}
typealias CSSM_KEYBLOB_TYPE = uint32
var CSSM_KEYBLOB_RAW: UInt32 { get }
var CSSM_KEYBLOB_REFERENCE: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED: UInt32 { get }
var CSSM_KEYBLOB_OTHER: UInt32 { get }
typealias CSSM_KEYBLOB_FORMAT = uint32
var CSSM_KEYBLOB_RAW_FORMAT_NONE: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_PKCS1: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_PKCS3: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_MSCAPI: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_PGP: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_FIPS186: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_BSAFE: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_CCA: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_PKCS8: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_SPKI: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_OCTET_STRING: UInt32 { get }
var CSSM_KEYBLOB_RAW_FORMAT_OTHER: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_NONE: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_PKCS8: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_PKCS7: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_MSCAPI: UInt32 { get }
var CSSM_KEYBLOB_WRAPPED_FORMAT_OTHER: UInt32 { get }
var CSSM_KEYBLOB_REF_FORMAT_INTEGER: UInt32 { get }
var CSSM_KEYBLOB_REF_FORMAT_STRING: UInt32 { get }
var CSSM_KEYBLOB_REF_FORMAT_SPKI: UInt32 { get }
var CSSM_KEYBLOB_REF_FORMAT_OTHER: UInt32 { get }
typealias CSSM_KEYCLASS = uint32
var CSSM_KEYCLASS_PUBLIC_KEY: UInt32 { get }
var CSSM_KEYCLASS_PRIVATE_KEY: UInt32 { get }
var CSSM_KEYCLASS_SESSION_KEY: UInt32 { get }
var CSSM_KEYCLASS_SECRET_PART: UInt32 { get }
var CSSM_KEYCLASS_OTHER: UInt32 { get }
typealias CSSM_KEYATTR_FLAGS = uint32
var CSSM_KEYATTR_RETURN_DEFAULT: Int { get }
var CSSM_KEYATTR_RETURN_DATA: Int { get }
var CSSM_KEYATTR_RETURN_REF: Int { get }
var CSSM_KEYATTR_RETURN_NONE: Int { get }
var CSSM_KEYATTR_PERMANENT: Int { get }
var CSSM_KEYATTR_PRIVATE: Int { get }
var CSSM_KEYATTR_MODIFIABLE: Int { get }
var CSSM_KEYATTR_SENSITIVE: Int { get }
var CSSM_KEYATTR_EXTRACTABLE: Int { get }
var CSSM_KEYATTR_ALWAYS_SENSITIVE: Int { get }
var CSSM_KEYATTR_NEVER_EXTRACTABLE: Int { get }
typealias CSSM_KEYUSE = uint32
var CSSM_KEYUSE_ANY: UInt32 { get }
var CSSM_KEYUSE_ENCRYPT: UInt32 { get }
var CSSM_KEYUSE_DECRYPT: UInt32 { get }
var CSSM_KEYUSE_SIGN: UInt32 { get }
var CSSM_KEYUSE_VERIFY: UInt32 { get }
var CSSM_KEYUSE_SIGN_RECOVER: UInt32 { get }
var CSSM_KEYUSE_VERIFY_RECOVER: UInt32 { get }
var CSSM_KEYUSE_WRAP: UInt32 { get }
var CSSM_KEYUSE_UNWRAP: UInt32 { get }
var CSSM_KEYUSE_DERIVE: UInt32 { get }
typealias CSSM_ALGORITHMS = uint32
var CSSM_ALGID_NONE: UInt32 { get }
var CSSM_ALGID_CUSTOM: UInt32 { get }
var CSSM_ALGID_DH: UInt32 { get }
var CSSM_ALGID_PH: UInt32 { get }
var CSSM_ALGID_KEA: UInt32 { get }
var CSSM_ALGID_MD2: UInt32 { get }
var CSSM_ALGID_MD4: UInt32 { get }
var CSSM_ALGID_MD5: UInt32 { get }
var CSSM_ALGID_SHA1: UInt32 { get }
var CSSM_ALGID_NHASH: UInt32 { get }
var CSSM_ALGID_HAVAL: UInt32 { get }
var CSSM_ALGID_RIPEMD: UInt32 { get }
var CSSM_ALGID_IBCHASH: UInt32 { get }
var CSSM_ALGID_RIPEMAC: UInt32 { get }
var CSSM_ALGID_DES: UInt32 { get }
var CSSM_ALGID_DESX: UInt32 { get }
var CSSM_ALGID_RDES: UInt32 { get }
var CSSM_ALGID_3DES_3KEY_EDE: UInt32 { get }
var CSSM_ALGID_3DES_2KEY_EDE: UInt32 { get }
var CSSM_ALGID_3DES_1KEY_EEE: UInt32 { get }
var CSSM_ALGID_3DES_3KEY: UInt32 { get }
var CSSM_ALGID_3DES_3KEY_EEE: UInt32 { get }
var CSSM_ALGID_3DES_2KEY: UInt32 { get }
var CSSM_ALGID_3DES_2KEY_EEE: UInt32 { get }
var CSSM_ALGID_3DES_1KEY: UInt32 { get }
var CSSM_ALGID_IDEA: UInt32 { get }
var CSSM_ALGID_RC2: UInt32 { get }
var CSSM_ALGID_RC5: UInt32 { get }
var CSSM_ALGID_RC4: UInt32 { get }
var CSSM_ALGID_SEAL: UInt32 { get }
var CSSM_ALGID_CAST: UInt32 { get }
var CSSM_ALGID_BLOWFISH: UInt32 { get }
var CSSM_ALGID_SKIPJACK: UInt32 { get }
var CSSM_ALGID_LUCIFER: UInt32 { get }
var CSSM_ALGID_MADRYGA: UInt32 { get }
var CSSM_ALGID_FEAL: UInt32 { get }
var CSSM_ALGID_REDOC: UInt32 { get }
var CSSM_ALGID_REDOC3: UInt32 { get }
var CSSM_ALGID_LOKI: UInt32 { get }
var CSSM_ALGID_KHUFU: UInt32 { get }
var CSSM_ALGID_KHAFRE: UInt32 { get }
var CSSM_ALGID_MMB: UInt32 { get }
var CSSM_ALGID_GOST: UInt32 { get }
var CSSM_ALGID_SAFER: UInt32 { get }
var CSSM_ALGID_CRAB: UInt32 { get }
var CSSM_ALGID_RSA: UInt32 { get }
var CSSM_ALGID_DSA: UInt32 { get }
var CSSM_ALGID_MD5WithRSA: UInt32 { get }
var CSSM_ALGID_MD2WithRSA: UInt32 { get }
var CSSM_ALGID_ElGamal: UInt32 { get }
var CSSM_ALGID_MD2Random: UInt32 { get }
var CSSM_ALGID_MD5Random: UInt32 { get }
var CSSM_ALGID_SHARandom: UInt32 { get }
var CSSM_ALGID_DESRandom: UInt32 { get }
var CSSM_ALGID_SHA1WithRSA: UInt32 { get }
var CSSM_ALGID_CDMF: UInt32 { get }
var CSSM_ALGID_CAST3: UInt32 { get }
var CSSM_ALGID_CAST5: UInt32 { get }
var CSSM_ALGID_GenericSecret: UInt32 { get }
var CSSM_ALGID_ConcatBaseAndKey: UInt32 { get }
var CSSM_ALGID_ConcatKeyAndBase: UInt32 { get }
var CSSM_ALGID_ConcatBaseAndData: UInt32 { get }
var CSSM_ALGID_ConcatDataAndBase: UInt32 { get }
var CSSM_ALGID_XORBaseAndData: UInt32 { get }
var CSSM_ALGID_ExtractFromKey: UInt32 { get }
var CSSM_ALGID_SSL3PreMasterGen: UInt32 { get }
var CSSM_ALGID_SSL3MasterDerive: UInt32 { get }
var CSSM_ALGID_SSL3KeyAndMacDerive: UInt32 { get }
var CSSM_ALGID_SSL3MD5_MAC: UInt32 { get }
var CSSM_ALGID_SSL3SHA1_MAC: UInt32 { get }
var CSSM_ALGID_PKCS5_PBKDF1_MD5: UInt32 { get }
var CSSM_ALGID_PKCS5_PBKDF1_MD2: UInt32 { get }
var CSSM_ALGID_PKCS5_PBKDF1_SHA1: UInt32 { get }
var CSSM_ALGID_WrapLynks: UInt32 { get }
var CSSM_ALGID_WrapSET_OAEP: UInt32 { get }
var CSSM_ALGID_BATON: UInt32 { get }
var CSSM_ALGID_ECDSA: UInt32 { get }
var CSSM_ALGID_MAYFLY: UInt32 { get }
var CSSM_ALGID_JUNIPER: UInt32 { get }
var CSSM_ALGID_FASTHASH: UInt32 { get }
var CSSM_ALGID_3DES: UInt32 { get }
var CSSM_ALGID_SSL3MD5: UInt32 { get }
var CSSM_ALGID_SSL3SHA1: UInt32 { get }
var CSSM_ALGID_FortezzaTimestamp: UInt32 { get }
var CSSM_ALGID_SHA1WithDSA: UInt32 { get }
var CSSM_ALGID_SHA1WithECDSA: UInt32 { get }
var CSSM_ALGID_DSA_BSAFE: UInt32 { get }
var CSSM_ALGID_ECDH: UInt32 { get }
var CSSM_ALGID_ECMQV: UInt32 { get }
var CSSM_ALGID_PKCS12_SHA1_PBE: UInt32 { get }
var CSSM_ALGID_ECNRA: UInt32 { get }
var CSSM_ALGID_SHA1WithECNRA: UInt32 { get }
var CSSM_ALGID_ECES: UInt32 { get }
var CSSM_ALGID_ECAES: UInt32 { get }
var CSSM_ALGID_SHA1HMAC: UInt32 { get }
var CSSM_ALGID_FIPS186Random: UInt32 { get }
var CSSM_ALGID_ECC: UInt32 { get }
var CSSM_ALGID_MQV: UInt32 { get }
var CSSM_ALGID_NRA: UInt32 { get }
var CSSM_ALGID_IntelPlatformRandom: UInt32 { get }
var CSSM_ALGID_UTC: UInt32 { get }
var CSSM_ALGID_HAVAL3: UInt32 { get }
var CSSM_ALGID_HAVAL4: UInt32 { get }
var CSSM_ALGID_HAVAL5: UInt32 { get }
var CSSM_ALGID_TIGER: UInt32 { get }
var CSSM_ALGID_MD5HMAC: UInt32 { get }
var CSSM_ALGID_PKCS5_PBKDF2: UInt32 { get }
var CSSM_ALGID_RUNNING_COUNTER: UInt32 { get }
var CSSM_ALGID_LAST: UInt32 { get }
var CSSM_ALGID_VENDOR_DEFINED: UInt32 { get }
typealias CSSM_ENCRYPT_MODE = uint32
var CSSM_ALGMODE_NONE: UInt32 { get }
var CSSM_ALGMODE_CUSTOM: UInt32 { get }
var CSSM_ALGMODE_ECB: UInt32 { get }
var CSSM_ALGMODE_ECBPad: UInt32 { get }
var CSSM_ALGMODE_CBC: UInt32 { get }
var CSSM_ALGMODE_CBC_IV8: UInt32 { get }
var CSSM_ALGMODE_CBCPadIV8: UInt32 { get }
var CSSM_ALGMODE_CFB: UInt32 { get }
var CSSM_ALGMODE_CFB_IV8: UInt32 { get }
var CSSM_ALGMODE_CFBPadIV8: UInt32 { get }
var CSSM_ALGMODE_OFB: UInt32 { get }
var CSSM_ALGMODE_OFB_IV8: UInt32 { get }
var CSSM_ALGMODE_OFBPadIV8: UInt32 { get }
var CSSM_ALGMODE_COUNTER: UInt32 { get }
var CSSM_ALGMODE_BC: UInt32 { get }
var CSSM_ALGMODE_PCBC: UInt32 { get }
var CSSM_ALGMODE_CBCC: UInt32 { get }
var CSSM_ALGMODE_OFBNLF: UInt32 { get }
var CSSM_ALGMODE_PBC: UInt32 { get }
var CSSM_ALGMODE_PFB: UInt32 { get }
var CSSM_ALGMODE_CBCPD: UInt32 { get }
var CSSM_ALGMODE_PUBLIC_KEY: UInt32 { get }
var CSSM_ALGMODE_PRIVATE_KEY: UInt32 { get }
var CSSM_ALGMODE_SHUFFLE: UInt32 { get }
var CSSM_ALGMODE_ECB64: UInt32 { get }
var CSSM_ALGMODE_CBC64: UInt32 { get }
var CSSM_ALGMODE_OFB64: UInt32 { get }
var CSSM_ALGMODE_CFB32: UInt32 { get }
var CSSM_ALGMODE_CFB16: UInt32 { get }
var CSSM_ALGMODE_CFB8: UInt32 { get }
var CSSM_ALGMODE_WRAP: UInt32 { get }
var CSSM_ALGMODE_PRIVATE_WRAP: UInt32 { get }
var CSSM_ALGMODE_RELAYX: UInt32 { get }
var CSSM_ALGMODE_ECB128: UInt32 { get }
var CSSM_ALGMODE_ECB96: UInt32 { get }
var CSSM_ALGMODE_CBC128: UInt32 { get }
var CSSM_ALGMODE_OAEP_HASH: UInt32 { get }
var CSSM_ALGMODE_PKCS1_EME_V15: UInt32 { get }
var CSSM_ALGMODE_PKCS1_EME_OAEP: UInt32 { get }
var CSSM_ALGMODE_PKCS1_EMSA_V15: UInt32 { get }
var CSSM_ALGMODE_ISO_9796: UInt32 { get }
var CSSM_ALGMODE_X9_31: UInt32 { get }
var CSSM_ALGMODE_LAST: UInt32 { get }
var CSSM_ALGMODE_VENDOR_DEFINED: UInt32 { get }
struct cssm_keyheader {
  var HeaderVersion: CSSM_HEADERVERSION
  var CspId: cssm_guid
  var BlobType: CSSM_KEYBLOB_TYPE
  var Format: CSSM_KEYBLOB_FORMAT
  var AlgorithmId: CSSM_ALGORITHMS
  var KeyClass: CSSM_KEYCLASS
  var LogicalKeySizeInBits: uint32
  var KeyAttr: CSSM_KEYATTR_FLAGS
  var KeyUsage: CSSM_KEYUSE
  var StartDate: cssm_date
  var EndDate: cssm_date
  var WrapAlgorithmId: CSSM_ALGORITHMS
  var WrapMode: CSSM_ENCRYPT_MODE
  var Reserved: uint32
  init()
  init(HeaderVersion HeaderVersion: CSSM_HEADERVERSION, CspId CspId: cssm_guid, BlobType BlobType: CSSM_KEYBLOB_TYPE, Format Format: CSSM_KEYBLOB_FORMAT, AlgorithmId AlgorithmId: CSSM_ALGORITHMS, KeyClass KeyClass: CSSM_KEYCLASS, LogicalKeySizeInBits LogicalKeySizeInBits: uint32, KeyAttr KeyAttr: CSSM_KEYATTR_FLAGS, KeyUsage KeyUsage: CSSM_KEYUSE, StartDate StartDate: cssm_date, EndDate EndDate: cssm_date, WrapAlgorithmId WrapAlgorithmId: CSSM_ALGORITHMS, WrapMode WrapMode: CSSM_ENCRYPT_MODE, Reserved Reserved: uint32)
}
struct cssm_key {
  var KeyHeader: cssm_keyheader
  var KeyData: cssm_data
  init()
  init(KeyHeader KeyHeader: cssm_keyheader, KeyData KeyData: cssm_data)
}
typealias CSSM_WRAP_KEY = cssm_key
typealias CSSM_WRAP_KEY_PTR = UnsafeMutablePointer<cssm_key>
typealias CSSM_CSPTYPE = uint32
var CSSM_CSP_SOFTWARE: Int { get }
var CSSM_CSP_HARDWARE: Int { get }
var CSSM_CSP_HYBRID: Int { get }
struct cssm_dl_db_handle {
  var DLHandle: CSSM_DL_HANDLE
  var DBHandle: CSSM_DB_HANDLE
  init()
  init(DLHandle DLHandle: CSSM_DL_HANDLE, DBHandle DBHandle: CSSM_DB_HANDLE)
}
typealias CSSM_CONTEXT_TYPE = uint32
var CSSM_ALGCLASS_NONE: Int { get }
var CSSM_ALGCLASS_CUSTOM: Int { get }
var CSSM_ALGCLASS_SIGNATURE: Int { get }
var CSSM_ALGCLASS_SYMMETRIC: Int { get }
var CSSM_ALGCLASS_DIGEST: Int { get }
var CSSM_ALGCLASS_RANDOMGEN: Int { get }
var CSSM_ALGCLASS_UNIQUEGEN: Int { get }
var CSSM_ALGCLASS_MAC: Int { get }
var CSSM_ALGCLASS_ASYMMETRIC: Int { get }
var CSSM_ALGCLASS_KEYGEN: Int { get }
var CSSM_ALGCLASS_DERIVEKEY: Int { get }
var CSSM_ATTRIBUTE_DATA_NONE: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_UINT32: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_CSSM_DATA: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_CRYPTO_DATA: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_KEY: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_STRING: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_DATE: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_RANGE: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_ACCESS_CREDENTIALS: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_VERSION: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_DL_DB_HANDLE: UInt32 { get }
var CSSM_ATTRIBUTE_DATA_KR_PROFILE: UInt32 { get }
var CSSM_ATTRIBUTE_TYPE_MASK: UInt32 { get }
typealias CSSM_ATTRIBUTE_TYPE = uint32
var CSSM_ATTRIBUTE_NONE: UInt32 { get }
var CSSM_ATTRIBUTE_CUSTOM: UInt32 { get }
var CSSM_ATTRIBUTE_DESCRIPTION: UInt32 { get }
var CSSM_ATTRIBUTE_KEY: UInt32 { get }
var CSSM_ATTRIBUTE_INIT_VECTOR: UInt32 { get }
var CSSM_ATTRIBUTE_SALT: UInt32 { get }
var CSSM_ATTRIBUTE_PADDING: UInt32 { get }
var CSSM_ATTRIBUTE_RANDOM: UInt32 { get }
var CSSM_ATTRIBUTE_SEED: UInt32 { get }
var CSSM_ATTRIBUTE_PASSPHRASE: UInt32 { get }
var CSSM_ATTRIBUTE_KEY_LENGTH: UInt32 { get }
var CSSM_ATTRIBUTE_KEY_LENGTH_RANGE: UInt32 { get }
var CSSM_ATTRIBUTE_BLOCK_SIZE: UInt32 { get }
var CSSM_ATTRIBUTE_OUTPUT_SIZE: UInt32 { get }
var CSSM_ATTRIBUTE_ROUNDS: UInt32 { get }
var CSSM_ATTRIBUTE_IV_SIZE: UInt32 { get }
var CSSM_ATTRIBUTE_ALG_PARAMS: UInt32 { get }
var CSSM_ATTRIBUTE_LABEL: UInt32 { get }
var CSSM_ATTRIBUTE_KEY_TYPE: UInt32 { get }
var CSSM_ATTRIBUTE_MODE: UInt32 { get }
var CSSM_ATTRIBUTE_EFFECTIVE_BITS: UInt32 { get }
var CSSM_ATTRIBUTE_START_DATE: UInt32 { get }
var CSSM_ATTRIBUTE_END_DATE: UInt32 { get }
var CSSM_ATTRIBUTE_KEYUSAGE: UInt32 { get }
var CSSM_ATTRIBUTE_KEYATTR: UInt32 { get }
var CSSM_ATTRIBUTE_VERSION: UInt32 { get }
var CSSM_ATTRIBUTE_PRIME: UInt32 { get }
var CSSM_ATTRIBUTE_BASE: UInt32 { get }
var CSSM_ATTRIBUTE_SUBPRIME: UInt32 { get }
var CSSM_ATTRIBUTE_ALG_ID: UInt32 { get }
var CSSM_ATTRIBUTE_ITERATION_COUNT: UInt32 { get }
var CSSM_ATTRIBUTE_ROUNDS_RANGE: UInt32 { get }
var CSSM_ATTRIBUTE_KRPROFILE_LOCAL: UInt32 { get }
var CSSM_ATTRIBUTE_KRPROFILE_REMOTE: UInt32 { get }
var CSSM_ATTRIBUTE_CSP_HANDLE: UInt32 { get }
var CSSM_ATTRIBUTE_DL_DB_HANDLE: UInt32 { get }
var CSSM_ATTRIBUTE_ACCESS_CREDENTIALS: UInt32 { get }
var CSSM_ATTRIBUTE_PUBLIC_KEY_FORMAT: UInt32 { get }
var CSSM_ATTRIBUTE_PRIVATE_KEY_FORMAT: UInt32 { get }
var CSSM_ATTRIBUTE_SYMMETRIC_KEY_FORMAT: UInt32 { get }
var CSSM_ATTRIBUTE_WRAPPED_KEY_FORMAT: UInt32 { get }
typealias CSSM_PADDING = uint32
var CSSM_PADDING_NONE: UInt32 { get }
var CSSM_PADDING_CUSTOM: UInt32 { get }
var CSSM_PADDING_ZERO: UInt32 { get }
var CSSM_PADDING_ONE: UInt32 { get }
var CSSM_PADDING_ALTERNATE: UInt32 { get }
var CSSM_PADDING_FF: UInt32 { get }
var CSSM_PADDING_PKCS5: UInt32 { get }
var CSSM_PADDING_PKCS7: UInt32 { get }
var CSSM_PADDING_CIPHERSTEALING: UInt32 { get }
var CSSM_PADDING_RANDOM: UInt32 { get }
var CSSM_PADDING_PKCS1: UInt32 { get }
var CSSM_PADDING_SIGRAW: UInt32 { get }
var CSSM_PADDING_VENDOR_DEFINED: UInt32 { get }
typealias CSSM_KEY_TYPE = CSSM_ALGORITHMS
struct cssm_context_attribute {
  var AttributeType: CSSM_ATTRIBUTE_TYPE
  var AttributeLength: uint32
  var Attribute: cssm_context_attribute_value
  init()
  init(AttributeType AttributeType: CSSM_ATTRIBUTE_TYPE, AttributeLength AttributeLength: uint32, Attribute Attribute: cssm_context_attribute_value)
}
struct cssm_context_attribute_value {
  var String: UnsafeMutablePointer<Int8>!
  var Uint32: uint32
  var AccessCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  var Key: UnsafeMutablePointer<cssm_key>!
  var Data: UnsafeMutablePointer<cssm_data>!
  var Padding: CSSM_PADDING
  var Date: UnsafeMutablePointer<cssm_date>!
  var Range: UnsafeMutablePointer<cssm_range>!
  var CryptoData: UnsafeMutablePointer<cssm_crypto_data>!
  var Version: UnsafeMutablePointer<cssm_version>!
  var DLDBHandle: UnsafeMutablePointer<cssm_dl_db_handle>!
  var KRProfile: UnsafeMutablePointer<cssm_kr_profile>!
  init(String String: UnsafeMutablePointer<Int8>!)
  init(Uint32 Uint32: uint32)
  init(AccessCredentials AccessCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
  init(Key Key: UnsafeMutablePointer<cssm_key>!)
  init(Data Data: UnsafeMutablePointer<cssm_data>!)
  init(Padding Padding: CSSM_PADDING)
  init(Date Date: UnsafeMutablePointer<cssm_date>!)
  init(Range Range: UnsafeMutablePointer<cssm_range>!)
  init(CryptoData CryptoData: UnsafeMutablePointer<cssm_crypto_data>!)
  init(Version Version: UnsafeMutablePointer<cssm_version>!)
  init(DLDBHandle DLDBHandle: UnsafeMutablePointer<cssm_dl_db_handle>!)
  init(KRProfile KRProfile: UnsafeMutablePointer<cssm_kr_profile>!)
  init()
}
typealias CSSM_CONTEXT_ATTRIBUTE = cssm_context_attribute
typealias CSSM_CONTEXT_ATTRIBUTE_PTR = UnsafeMutablePointer<cssm_context_attribute>
struct cssm_context {
  var ContextType: CSSM_CONTEXT_TYPE
  var AlgorithmType: CSSM_ALGORITHMS
  var NumberOfAttributes: uint32
  var ContextAttributes: CSSM_CONTEXT_ATTRIBUTE_PTR!
  var CSPHandle: CSSM_CSP_HANDLE
  var Privileged: CSSM_BOOL
  var EncryptionProhibited: uint32
  var WorkFactor: uint32
  var Reserved: uint32
  init()
  init(ContextType ContextType: CSSM_CONTEXT_TYPE, AlgorithmType AlgorithmType: CSSM_ALGORITHMS, NumberOfAttributes NumberOfAttributes: uint32, ContextAttributes ContextAttributes: CSSM_CONTEXT_ATTRIBUTE_PTR!, CSPHandle CSPHandle: CSSM_CSP_HANDLE, Privileged Privileged: CSSM_BOOL, EncryptionProhibited EncryptionProhibited: uint32, WorkFactor WorkFactor: uint32, Reserved Reserved: uint32)
}
typealias CSSM_SC_FLAGS = uint32
var CSSM_CSP_TOK_RNG: Int { get }
var CSSM_CSP_TOK_CLOCK_EXISTS: Int { get }
typealias CSSM_CSP_READER_FLAGS = uint32
var CSSM_CSP_RDR_TOKENPRESENT: Int { get }
var CSSM_CSP_RDR_EXISTS: Int { get }
var CSSM_CSP_RDR_HW: Int { get }
typealias CSSM_CSP_FLAGS = uint32
var CSSM_CSP_TOK_WRITE_PROTECTED: Int { get }
var CSSM_CSP_TOK_LOGIN_REQUIRED: Int { get }
var CSSM_CSP_TOK_USER_PIN_INITIALIZED: Int { get }
var CSSM_CSP_TOK_PROT_AUTHENTICATION: Int { get }
var CSSM_CSP_TOK_USER_PIN_EXPIRED: Int { get }
var CSSM_CSP_TOK_SESSION_KEY_PASSWORD: Int { get }
var CSSM_CSP_TOK_PRIVATE_KEY_PASSWORD: Int { get }
var CSSM_CSP_STORES_PRIVATE_KEYS: Int { get }
var CSSM_CSP_STORES_PUBLIC_KEYS: Int { get }
var CSSM_CSP_STORES_SESSION_KEYS: Int { get }
var CSSM_CSP_STORES_CERTIFICATES: Int { get }
var CSSM_CSP_STORES_GENERIC: Int { get }
typealias CSSM_PKCS_OAEP_MGF = uint32
var CSSM_PKCS_OAEP_MGF_NONE: Int { get }
var CSSM_PKCS_OAEP_MGF1_SHA1: Int { get }
var CSSM_PKCS_OAEP_MGF1_MD5: Int { get }
typealias CSSM_PKCS_OAEP_PSOURCE = uint32
var CSSM_PKCS_OAEP_PSOURCE_NONE: Int { get }
var CSSM_PKCS_OAEP_PSOURCE_Pspecified: Int { get }
struct cssm_pkcs1_oaep_params {
  var HashAlgorithm: uint32
  var HashParams: cssm_data
  var MGF: CSSM_PKCS_OAEP_MGF
  var MGFParams: cssm_data
  var PSource: CSSM_PKCS_OAEP_PSOURCE
  var PSourceParams: cssm_data
  init()
  init(HashAlgorithm HashAlgorithm: uint32, HashParams HashParams: cssm_data, MGF MGF: CSSM_PKCS_OAEP_MGF, MGFParams MGFParams: cssm_data, PSource PSource: CSSM_PKCS_OAEP_PSOURCE, PSourceParams PSourceParams: cssm_data)
}
struct cssm_csp_operational_statistics {
  var UserAuthenticated: CSSM_BOOL
  var DeviceFlags: CSSM_CSP_FLAGS
  var TokenMaxSessionCount: uint32
  var TokenOpenedSessionCount: uint32
  var TokenMaxRWSessionCount: uint32
  var TokenOpenedRWSessionCount: uint32
  var TokenTotalPublicMem: uint32
  var TokenFreePublicMem: uint32
  var TokenTotalPrivateMem: uint32
  var TokenFreePrivateMem: uint32
  init()
  init(UserAuthenticated UserAuthenticated: CSSM_BOOL, DeviceFlags DeviceFlags: CSSM_CSP_FLAGS, TokenMaxSessionCount TokenMaxSessionCount: uint32, TokenOpenedSessionCount TokenOpenedSessionCount: uint32, TokenMaxRWSessionCount TokenMaxRWSessionCount: uint32, TokenOpenedRWSessionCount TokenOpenedRWSessionCount: uint32, TokenTotalPublicMem TokenTotalPublicMem: uint32, TokenFreePublicMem TokenFreePublicMem: uint32, TokenTotalPrivateMem TokenTotalPrivateMem: uint32, TokenFreePrivateMem TokenFreePrivateMem: uint32)
}
var CSSM_VALUE_NOT_AVAILABLE: Int { get }
struct cssm_pkcs5_pbkdf1_params {
  var Passphrase: cssm_data
  var InitVector: cssm_data
  init()
  init(Passphrase Passphrase: cssm_data, InitVector InitVector: cssm_data)
}
typealias CSSM_PKCS5_PBKDF2_PRF = uint32
var CSSM_PKCS5_PBKDF2_PRF_HMAC_SHA1: Int { get }
struct cssm_pkcs5_pbkdf2_params {
  var Passphrase: cssm_data
  var PseudoRandomFunction: CSSM_PKCS5_PBKDF2_PRF
  init()
  init(Passphrase Passphrase: cssm_data, PseudoRandomFunction PseudoRandomFunction: CSSM_PKCS5_PBKDF2_PRF)
}
struct cssm_kea_derive_params {
  var Rb: cssm_data
  var Yb: cssm_data
  init()
  init(Rb Rb: cssm_data, Yb Yb: cssm_data)
}
struct cssm_tp_authority_id {
  var AuthorityCert: UnsafeMutablePointer<cssm_data>!
  var AuthorityLocation: UnsafeMutablePointer<cssm_net_address>!
  init()
  init(AuthorityCert AuthorityCert: UnsafeMutablePointer<cssm_data>!, AuthorityLocation AuthorityLocation: UnsafeMutablePointer<cssm_net_address>!)
}
typealias CSSM_TP_AUTHORITY_REQUEST_TYPE = uint32
typealias CSSM_TP_AUTHORITY_REQUEST_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_TP_AUTHORITY_REQUEST_CERTISSUE: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTREVOKE: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTSUSPEND: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTRESUME: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTVERIFY: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTNOTARIZE: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CERTUSERECOVER: Int { get }
var CSSM_TP_AUTHORITY_REQUEST_CRLISSUE: Int { get }
typealias CSSM_TP_VERIFICATION_RESULTS_CALLBACK = @convention(c) (CSSM_MODULE_HANDLE, UnsafeMutablePointer<Void>!, UnsafeMutablePointer<cssm_data>!) -> CSSM_RETURN
typealias CSSM_OID = cssm_data
typealias CSSM_OID_PTR = UnsafeMutablePointer<cssm_data>
struct cssm_field {
  var FieldOid: CSSM_OID
  var FieldValue: cssm_data
  init()
  init(FieldOid FieldOid: CSSM_OID, FieldValue FieldValue: cssm_data)
}
struct cssm_tp_policyinfo {
  var NumberOfPolicyIds: uint32
  var PolicyIds: UnsafeMutablePointer<cssm_field>!
  var PolicyControl: UnsafeMutablePointer<Void>!
  init()
  init(NumberOfPolicyIds NumberOfPolicyIds: uint32, PolicyIds PolicyIds: UnsafeMutablePointer<cssm_field>!, PolicyControl PolicyControl: UnsafeMutablePointer<Void>!)
}
typealias CSSM_TP_SERVICES = uint32
var CSSM_TP_KEY_ARCHIVE: Int { get }
var CSSM_TP_CERT_PUBLISH: Int { get }
var CSSM_TP_CERT_NOTIFY_RENEW: Int { get }
var CSSM_TP_CERT_DIR_UPDATE: Int { get }
var CSSM_TP_CRL_DISTRIBUTE: Int { get }
typealias CSSM_TP_ACTION = uint32
var CSSM_TP_ACTION_DEFAULT: Int { get }
typealias CSSM_TP_STOP_ON = uint32
var CSSM_TP_STOP_ON_POLICY: Int { get }
var CSSM_TP_STOP_ON_NONE: Int { get }
var CSSM_TP_STOP_ON_FIRST_PASS: Int { get }
var CSSM_TP_STOP_ON_FIRST_FAIL: Int { get }
typealias CSSM_TIMESTRING = UnsafeMutablePointer<Int8>
struct cssm_dl_db_list {
  var NumHandles: uint32
  var DLDBHandle: UnsafeMutablePointer<cssm_dl_db_handle>!
  init()
  init(NumHandles NumHandles: uint32, DLDBHandle DLDBHandle: UnsafeMutablePointer<cssm_dl_db_handle>!)
}
struct cssm_tp_callerauth_context {
  var Policy: cssm_tp_policyinfo
  var VerifyTime: CSSM_TIMESTRING!
  var VerificationAbortOn: CSSM_TP_STOP_ON
  var CallbackWithVerifiedCert: CSSM_TP_VERIFICATION_RESULTS_CALLBACK!
  var NumberOfAnchorCerts: uint32
  var AnchorCerts: UnsafeMutablePointer<cssm_data>!
  var DBList: UnsafeMutablePointer<cssm_dl_db_list>!
  var CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(Policy Policy: cssm_tp_policyinfo, VerifyTime VerifyTime: CSSM_TIMESTRING!, VerificationAbortOn VerificationAbortOn: CSSM_TP_STOP_ON, CallbackWithVerifiedCert CallbackWithVerifiedCert: CSSM_TP_VERIFICATION_RESULTS_CALLBACK!, NumberOfAnchorCerts NumberOfAnchorCerts: uint32, AnchorCerts AnchorCerts: UnsafeMutablePointer<cssm_data>!, DBList DBList: UnsafeMutablePointer<cssm_dl_db_list>!, CallerCredentials CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_CRL_PARSE_FORMAT = uint32
typealias CSSM_CRL_PARSE_FORMAT_PTR = UnsafeMutablePointer<uint32>
var CSSM_CRL_PARSE_FORMAT_NONE: Int { get }
var CSSM_CRL_PARSE_FORMAT_CUSTOM: Int { get }
var CSSM_CRL_PARSE_FORMAT_SEXPR: Int { get }
var CSSM_CRL_PARSE_FORMAT_COMPLEX: Int { get }
var CSSM_CRL_PARSE_FORMAT_OID_NAMED: Int { get }
var CSSM_CRL_PARSE_FORMAT_TUPLE: Int { get }
var CSSM_CRL_PARSE_FORMAT_MULTIPLE: Int { get }
var CSSM_CRL_PARSE_FORMAT_LAST: Int { get }
var CSSM_CL_CUSTOM_CRL_PARSE_FORMAT: Int { get }
typealias CSSM_CRL_TYPE = uint32
typealias CSSM_CRL_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_CRL_TYPE_UNKNOWN: Int { get }
var CSSM_CRL_TYPE_X_509v1: Int { get }
var CSSM_CRL_TYPE_X_509v2: Int { get }
var CSSM_CRL_TYPE_SPKI: Int { get }
var CSSM_CRL_TYPE_MULTIPLE: Int { get }
typealias CSSM_CRL_ENCODING = uint32
typealias CSSM_CRL_ENCODING_PTR = UnsafeMutablePointer<uint32>
var CSSM_CRL_ENCODING_UNKNOWN: Int { get }
var CSSM_CRL_ENCODING_CUSTOM: Int { get }
var CSSM_CRL_ENCODING_BER: Int { get }
var CSSM_CRL_ENCODING_DER: Int { get }
var CSSM_CRL_ENCODING_BLOOM: Int { get }
var CSSM_CRL_ENCODING_SEXPR: Int { get }
var CSSM_CRL_ENCODING_MULTIPLE: Int { get }
struct cssm_encoded_crl {
  var CrlType: CSSM_CRL_TYPE
  var CrlEncoding: CSSM_CRL_ENCODING
  var CrlBlob: cssm_data
  init()
  init(CrlType CrlType: CSSM_CRL_TYPE, CrlEncoding CrlEncoding: CSSM_CRL_ENCODING, CrlBlob CrlBlob: cssm_data)
}
struct cssm_parsed_crl {
  var CrlType: CSSM_CRL_TYPE
  var ParsedCrlFormat: CSSM_CRL_PARSE_FORMAT
  var ParsedCrl: UnsafeMutablePointer<Void>!
  init()
  init(CrlType CrlType: CSSM_CRL_TYPE, ParsedCrlFormat ParsedCrlFormat: CSSM_CRL_PARSE_FORMAT, ParsedCrl ParsedCrl: UnsafeMutablePointer<Void>!)
}
struct cssm_crl_pair {
  var EncodedCrl: cssm_encoded_crl
  var ParsedCrl: cssm_parsed_crl
  init()
  init(EncodedCrl EncodedCrl: cssm_encoded_crl, ParsedCrl ParsedCrl: cssm_parsed_crl)
}
typealias CSSM_CRLGROUP_TYPE = uint32
typealias CSSM_CRLGROUP_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_CRLGROUP_DATA: Int { get }
var CSSM_CRLGROUP_ENCODED_CRL: Int { get }
var CSSM_CRLGROUP_PARSED_CRL: Int { get }
var CSSM_CRLGROUP_CRL_PAIR: Int { get }
struct cssm_crlgroup {
  struct __Unnamed_union_GroupCrlList {
    var CrlList: UnsafeMutablePointer<cssm_data>!
    var EncodedCrlList: UnsafeMutablePointer<cssm_encoded_crl>!
    var ParsedCrlList: UnsafeMutablePointer<cssm_parsed_crl>!
    var PairCrlList: UnsafeMutablePointer<cssm_crl_pair>!
    init(CrlList CrlList: UnsafeMutablePointer<cssm_data>!)
    init(EncodedCrlList EncodedCrlList: UnsafeMutablePointer<cssm_encoded_crl>!)
    init(ParsedCrlList ParsedCrlList: UnsafeMutablePointer<cssm_parsed_crl>!)
    init(PairCrlList PairCrlList: UnsafeMutablePointer<cssm_crl_pair>!)
    init()
  }
  var CrlType: CSSM_CRL_TYPE
  var CrlEncoding: CSSM_CRL_ENCODING
  var NumberOfCrls: uint32
  var GroupCrlList: cssm_crlgroup.__Unnamed_union_GroupCrlList
  var CrlGroupType: CSSM_CRLGROUP_TYPE
  init()
  init(CrlType CrlType: CSSM_CRL_TYPE, CrlEncoding CrlEncoding: CSSM_CRL_ENCODING, NumberOfCrls NumberOfCrls: uint32, GroupCrlList GroupCrlList: cssm_crlgroup.__Unnamed_union_GroupCrlList, CrlGroupType CrlGroupType: CSSM_CRLGROUP_TYPE)
}
typealias CSSM_CRLGROUP = cssm_crlgroup
typealias CSSM_CRLGROUP_PTR = UnsafeMutablePointer<cssm_crlgroup>
struct cssm_fieldgroup {
  var NumberOfFields: Int32
  var Fields: UnsafeMutablePointer<cssm_field>!
  init()
  init(NumberOfFields NumberOfFields: Int32, Fields Fields: UnsafeMutablePointer<cssm_field>!)
}
typealias CSSM_EVIDENCE_FORM = uint32
var CSSM_EVIDENCE_FORM_UNSPECIFIC: Int { get }
var CSSM_EVIDENCE_FORM_CERT: Int { get }
var CSSM_EVIDENCE_FORM_CRL: Int { get }
var CSSM_EVIDENCE_FORM_CERT_ID: Int { get }
var CSSM_EVIDENCE_FORM_CRL_ID: Int { get }
var CSSM_EVIDENCE_FORM_VERIFIER_TIME: Int { get }
var CSSM_EVIDENCE_FORM_CRL_THISTIME: Int { get }
var CSSM_EVIDENCE_FORM_CRL_NEXTTIME: Int { get }
var CSSM_EVIDENCE_FORM_POLICYINFO: Int { get }
var CSSM_EVIDENCE_FORM_TUPLEGROUP: Int { get }
struct cssm_evidence {
  var EvidenceForm: CSSM_EVIDENCE_FORM
  var Evidence: UnsafeMutablePointer<Void>!
  init()
  init(EvidenceForm EvidenceForm: CSSM_EVIDENCE_FORM, Evidence Evidence: UnsafeMutablePointer<Void>!)
}
struct cssm_tp_verify_context {
  var Action: CSSM_TP_ACTION
  var ActionData: cssm_data
  var Crls: CSSM_CRLGROUP
  var Cred: UnsafeMutablePointer<cssm_tp_callerauth_context>!
  init()
  init(Action Action: CSSM_TP_ACTION, ActionData ActionData: cssm_data, Crls Crls: CSSM_CRLGROUP, Cred Cred: UnsafeMutablePointer<cssm_tp_callerauth_context>!)
}
struct cssm_tp_verify_context_result {
  var NumberOfEvidences: uint32
  var Evidence: UnsafeMutablePointer<cssm_evidence>!
  init()
  init(NumberOfEvidences NumberOfEvidences: uint32, Evidence Evidence: UnsafeMutablePointer<cssm_evidence>!)
}
struct cssm_tp_request_set {
  var NumberOfRequests: uint32
  var Requests: UnsafeMutablePointer<Void>!
  init()
  init(NumberOfRequests NumberOfRequests: uint32, Requests Requests: UnsafeMutablePointer<Void>!)
}
struct cssm_tp_result_set {
  var NumberOfResults: uint32
  var Results: UnsafeMutablePointer<Void>!
  init()
  init(NumberOfResults NumberOfResults: uint32, Results Results: UnsafeMutablePointer<Void>!)
}
typealias CSSM_TP_CONFIRM_STATUS = uint32
typealias CSSM_TP_CONFIRM_STATUS_PTR = UnsafeMutablePointer<uint32>
var CSSM_TP_CONFIRM_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CONFIRM_ACCEPT: Int { get }
var CSSM_TP_CONFIRM_REJECT: Int { get }
struct cssm_tp_confirm_response {
  var NumberOfResponses: uint32
  var Responses: CSSM_TP_CONFIRM_STATUS_PTR!
  init()
  init(NumberOfResponses NumberOfResponses: uint32, Responses Responses: CSSM_TP_CONFIRM_STATUS_PTR!)
}
var CSSM_ESTIMATED_TIME_UNKNOWN: Int { get }
var CSSM_ELAPSED_TIME_UNKNOWN: Int { get }
var CSSM_ELAPSED_TIME_COMPLETE: Int { get }
struct cssm_tp_certissue_input {
  var CSPSubserviceUid: cssm_subservice_uid
  var CLHandle: CSSM_CL_HANDLE
  var NumberOfTemplateFields: uint32
  var SubjectCertFields: UnsafeMutablePointer<cssm_field>!
  var MoreServiceRequests: CSSM_TP_SERVICES
  var NumberOfServiceControls: uint32
  var ServiceControls: UnsafeMutablePointer<cssm_field>!
  var UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(CSPSubserviceUid CSPSubserviceUid: cssm_subservice_uid, CLHandle CLHandle: CSSM_CL_HANDLE, NumberOfTemplateFields NumberOfTemplateFields: uint32, SubjectCertFields SubjectCertFields: UnsafeMutablePointer<cssm_field>!, MoreServiceRequests MoreServiceRequests: CSSM_TP_SERVICES, NumberOfServiceControls NumberOfServiceControls: uint32, ServiceControls ServiceControls: UnsafeMutablePointer<cssm_field>!, UserCredentials UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_TP_CERTISSUE_STATUS = uint32
var CSSM_TP_CERTISSUE_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CERTISSUE_OK: Int { get }
var CSSM_TP_CERTISSUE_OKWITHCERTMODS: Int { get }
var CSSM_TP_CERTISSUE_OKWITHSERVICEMODS: Int { get }
var CSSM_TP_CERTISSUE_REJECTED: Int { get }
var CSSM_TP_CERTISSUE_NOT_AUTHORIZED: Int { get }
var CSSM_TP_CERTISSUE_WILL_BE_REVOKED: Int { get }
struct cssm_tp_certissue_output {
  var IssueStatus: CSSM_TP_CERTISSUE_STATUS
  var CertGroup: CSSM_CERTGROUP_PTR!
  var PerformedServiceRequests: CSSM_TP_SERVICES
  init()
  init(IssueStatus IssueStatus: CSSM_TP_CERTISSUE_STATUS, CertGroup CertGroup: CSSM_CERTGROUP_PTR!, PerformedServiceRequests PerformedServiceRequests: CSSM_TP_SERVICES)
}
typealias CSSM_TP_CERTCHANGE_ACTION = uint32
var CSSM_TP_CERTCHANGE_NONE: Int { get }
var CSSM_TP_CERTCHANGE_REVOKE: Int { get }
var CSSM_TP_CERTCHANGE_HOLD: Int { get }
var CSSM_TP_CERTCHANGE_RELEASE: Int { get }
typealias CSSM_TP_CERTCHANGE_REASON = uint32
var CSSM_TP_CERTCHANGE_REASON_UNKNOWN: Int { get }
var CSSM_TP_CERTCHANGE_REASON_KEYCOMPROMISE: Int { get }
var CSSM_TP_CERTCHANGE_REASON_CACOMPROMISE: Int { get }
var CSSM_TP_CERTCHANGE_REASON_CEASEOPERATION: Int { get }
var CSSM_TP_CERTCHANGE_REASON_AFFILIATIONCHANGE: Int { get }
var CSSM_TP_CERTCHANGE_REASON_SUPERCEDED: Int { get }
var CSSM_TP_CERTCHANGE_REASON_SUSPECTEDCOMPROMISE: Int { get }
var CSSM_TP_CERTCHANGE_REASON_HOLDRELEASE: Int { get }
struct cssm_tp_certchange_input {
  var Action: CSSM_TP_CERTCHANGE_ACTION
  var Reason: CSSM_TP_CERTCHANGE_REASON
  var CLHandle: CSSM_CL_HANDLE
  var Cert: UnsafeMutablePointer<cssm_data>!
  var ChangeInfo: UnsafeMutablePointer<cssm_field>!
  var StartTime: CSSM_TIMESTRING!
  var CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(Action Action: CSSM_TP_CERTCHANGE_ACTION, Reason Reason: CSSM_TP_CERTCHANGE_REASON, CLHandle CLHandle: CSSM_CL_HANDLE, Cert Cert: UnsafeMutablePointer<cssm_data>!, ChangeInfo ChangeInfo: UnsafeMutablePointer<cssm_field>!, StartTime StartTime: CSSM_TIMESTRING!, CallerCredentials CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_TP_CERTCHANGE_STATUS = uint32
var CSSM_TP_CERTCHANGE_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CERTCHANGE_OK: Int { get }
var CSSM_TP_CERTCHANGE_OKWITHNEWTIME: Int { get }
var CSSM_TP_CERTCHANGE_WRONGCA: Int { get }
var CSSM_TP_CERTCHANGE_REJECTED: Int { get }
var CSSM_TP_CERTCHANGE_NOT_AUTHORIZED: Int { get }
struct cssm_tp_certchange_output {
  var ActionStatus: CSSM_TP_CERTCHANGE_STATUS
  var RevokeInfo: cssm_field
  init()
  init(ActionStatus ActionStatus: CSSM_TP_CERTCHANGE_STATUS, RevokeInfo RevokeInfo: cssm_field)
}
struct cssm_tp_certverify_input {
  var CLHandle: CSSM_CL_HANDLE
  var Cert: UnsafeMutablePointer<cssm_data>!
  var VerifyContext: UnsafeMutablePointer<cssm_tp_verify_context>!
  init()
  init(CLHandle CLHandle: CSSM_CL_HANDLE, Cert Cert: UnsafeMutablePointer<cssm_data>!, VerifyContext VerifyContext: UnsafeMutablePointer<cssm_tp_verify_context>!)
}
typealias CSSM_TP_CERTVERIFY_STATUS = uint32
var CSSM_TP_CERTVERIFY_UNKNOWN: Int { get }
var CSSM_TP_CERTVERIFY_VALID: Int { get }
var CSSM_TP_CERTVERIFY_INVALID: Int { get }
var CSSM_TP_CERTVERIFY_REVOKED: Int { get }
var CSSM_TP_CERTVERIFY_SUSPENDED: Int { get }
var CSSM_TP_CERTVERIFY_EXPIRED: Int { get }
var CSSM_TP_CERTVERIFY_NOT_VALID_YET: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_AUTHORITY: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_SIGNATURE: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_CERT_VALUE: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_CERTGROUP: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_POLICY: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_POLICY_IDS: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_BASIC_CONSTRAINTS: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_CRL_DIST_PT: Int { get }
var CSSM_TP_CERTVERIFY_INVALID_NAME_TREE: Int { get }
var CSSM_TP_CERTVERIFY_UNKNOWN_CRITICAL_EXT: Int { get }
struct cssm_tp_certverify_output {
  var VerifyStatus: CSSM_TP_CERTVERIFY_STATUS
  var NumberOfEvidence: uint32
  var Evidence: UnsafeMutablePointer<cssm_evidence>!
  init()
  init(VerifyStatus VerifyStatus: CSSM_TP_CERTVERIFY_STATUS, NumberOfEvidence NumberOfEvidence: uint32, Evidence Evidence: UnsafeMutablePointer<cssm_evidence>!)
}
struct cssm_tp_certnotarize_input {
  var CLHandle: CSSM_CL_HANDLE
  var NumberOfFields: uint32
  var MoreFields: UnsafeMutablePointer<cssm_field>!
  var SignScope: UnsafeMutablePointer<cssm_field>!
  var ScopeSize: uint32
  var MoreServiceRequests: CSSM_TP_SERVICES
  var NumberOfServiceControls: uint32
  var ServiceControls: UnsafeMutablePointer<cssm_field>!
  var UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(CLHandle CLHandle: CSSM_CL_HANDLE, NumberOfFields NumberOfFields: uint32, MoreFields MoreFields: UnsafeMutablePointer<cssm_field>!, SignScope SignScope: UnsafeMutablePointer<cssm_field>!, ScopeSize ScopeSize: uint32, MoreServiceRequests MoreServiceRequests: CSSM_TP_SERVICES, NumberOfServiceControls NumberOfServiceControls: uint32, ServiceControls ServiceControls: UnsafeMutablePointer<cssm_field>!, UserCredentials UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_TP_CERTNOTARIZE_STATUS = uint32
var CSSM_TP_CERTNOTARIZE_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CERTNOTARIZE_OK: Int { get }
var CSSM_TP_CERTNOTARIZE_OKWITHOUTFIELDS: Int { get }
var CSSM_TP_CERTNOTARIZE_OKWITHSERVICEMODS: Int { get }
var CSSM_TP_CERTNOTARIZE_REJECTED: Int { get }
var CSSM_TP_CERTNOTARIZE_NOT_AUTHORIZED: Int { get }
struct cssm_tp_certnotarize_output {
  var NotarizeStatus: CSSM_TP_CERTNOTARIZE_STATUS
  var NotarizedCertGroup: CSSM_CERTGROUP_PTR!
  var PerformedServiceRequests: CSSM_TP_SERVICES
  init()
  init(NotarizeStatus NotarizeStatus: CSSM_TP_CERTNOTARIZE_STATUS, NotarizedCertGroup NotarizedCertGroup: CSSM_CERTGROUP_PTR!, PerformedServiceRequests PerformedServiceRequests: CSSM_TP_SERVICES)
}
struct cssm_tp_certreclaim_input {
  var CLHandle: CSSM_CL_HANDLE
  var NumberOfSelectionFields: uint32
  var SelectionFields: UnsafeMutablePointer<cssm_field>!
  var UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(CLHandle CLHandle: CSSM_CL_HANDLE, NumberOfSelectionFields NumberOfSelectionFields: uint32, SelectionFields SelectionFields: UnsafeMutablePointer<cssm_field>!, UserCredentials UserCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_TP_CERTRECLAIM_STATUS = uint32
var CSSM_TP_CERTRECLAIM_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CERTRECLAIM_OK: Int { get }
var CSSM_TP_CERTRECLAIM_NOMATCH: Int { get }
var CSSM_TP_CERTRECLAIM_REJECTED: Int { get }
var CSSM_TP_CERTRECLAIM_NOT_AUTHORIZED: Int { get }
struct cssm_tp_certreclaim_output {
  var ReclaimStatus: CSSM_TP_CERTRECLAIM_STATUS
  var ReclaimedCertGroup: CSSM_CERTGROUP_PTR!
  var KeyCacheHandle: CSSM_LONG_HANDLE
  init()
  init(ReclaimStatus ReclaimStatus: CSSM_TP_CERTRECLAIM_STATUS, ReclaimedCertGroup ReclaimedCertGroup: CSSM_CERTGROUP_PTR!, KeyCacheHandle KeyCacheHandle: CSSM_LONG_HANDLE)
}
struct cssm_tp_crlissue_input {
  var CLHandle: CSSM_CL_HANDLE
  var CrlIdentifier: uint32
  var CrlThisTime: CSSM_TIMESTRING!
  var PolicyIdentifier: UnsafeMutablePointer<cssm_field>!
  var CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!
  init()
  init(CLHandle CLHandle: CSSM_CL_HANDLE, CrlIdentifier CrlIdentifier: uint32, CrlThisTime CrlThisTime: CSSM_TIMESTRING!, PolicyIdentifier PolicyIdentifier: UnsafeMutablePointer<cssm_field>!, CallerCredentials CallerCredentials: UnsafeMutablePointer<cssm_access_credentials>!)
}
typealias CSSM_TP_CRLISSUE_STATUS = uint32
var CSSM_TP_CRLISSUE_STATUS_UNKNOWN: Int { get }
var CSSM_TP_CRLISSUE_OK: Int { get }
var CSSM_TP_CRLISSUE_NOT_CURRENT: Int { get }
var CSSM_TP_CRLISSUE_INVALID_DOMAIN: Int { get }
var CSSM_TP_CRLISSUE_UNKNOWN_IDENTIFIER: Int { get }
var CSSM_TP_CRLISSUE_REJECTED: Int { get }
var CSSM_TP_CRLISSUE_NOT_AUTHORIZED: Int { get }
struct cssm_tp_crlissue_output {
  var IssueStatus: CSSM_TP_CRLISSUE_STATUS
  var Crl: UnsafeMutablePointer<cssm_encoded_crl>!
  var CrlNextTime: CSSM_TIMESTRING!
  init()
  init(IssueStatus IssueStatus: CSSM_TP_CRLISSUE_STATUS, Crl Crl: UnsafeMutablePointer<cssm_encoded_crl>!, CrlNextTime CrlNextTime: CSSM_TIMESTRING!)
}
typealias CSSM_TP_FORM_TYPE = uint32
var CSSM_TP_FORM_TYPE_GENERIC: Int { get }
var CSSM_TP_FORM_TYPE_REGISTRATION: Int { get }
typealias CSSM_CL_TEMPLATE_TYPE = uint32
var CSSM_CL_TEMPLATE_INTERMEDIATE_CERT: Int { get }
var CSSM_CL_TEMPLATE_PKIX_CERTTEMPLATE: Int { get }
typealias CSSM_CERT_BUNDLE_TYPE = uint32
var CSSM_CERT_BUNDLE_UNKNOWN: Int { get }
var CSSM_CERT_BUNDLE_CUSTOM: Int { get }
var CSSM_CERT_BUNDLE_PKCS7_SIGNED_DATA: Int { get }
var CSSM_CERT_BUNDLE_PKCS7_SIGNED_ENVELOPED_DATA: Int { get }
var CSSM_CERT_BUNDLE_PKCS12: Int { get }
var CSSM_CERT_BUNDLE_PFX: Int { get }
var CSSM_CERT_BUNDLE_SPKI_SEQUENCE: Int { get }
var CSSM_CERT_BUNDLE_PGP_KEYRING: Int { get }
var CSSM_CERT_BUNDLE_LAST: Int { get }
var CSSM_CL_CUSTOM_CERT_BUNDLE_TYPE: Int { get }
typealias CSSM_CERT_BUNDLE_ENCODING = uint32
var CSSM_CERT_BUNDLE_ENCODING_UNKNOWN: Int { get }
var CSSM_CERT_BUNDLE_ENCODING_CUSTOM: Int { get }
var CSSM_CERT_BUNDLE_ENCODING_BER: Int { get }
var CSSM_CERT_BUNDLE_ENCODING_DER: Int { get }
var CSSM_CERT_BUNDLE_ENCODING_SEXPR: Int { get }
var CSSM_CERT_BUNDLE_ENCODING_PGP: Int { get }
struct cssm_cert_bundle_header {
  var BundleType: CSSM_CERT_BUNDLE_TYPE
  var BundleEncoding: CSSM_CERT_BUNDLE_ENCODING
  init()
  init(BundleType BundleType: CSSM_CERT_BUNDLE_TYPE, BundleEncoding BundleEncoding: CSSM_CERT_BUNDLE_ENCODING)
}
struct cssm_cert_bundle {
  var BundleHeader: cssm_cert_bundle_header
  var Bundle: cssm_data
  init()
  init(BundleHeader BundleHeader: cssm_cert_bundle_header, Bundle Bundle: cssm_data)
}
var CSSM_FIELDVALUE_COMPLEX_DATA_TYPE: UInt32 { get }
typealias CSSM_DB_ATTRIBUTE_NAME_FORMAT = uint32
typealias CSSM_DB_ATTRIBUTE_NAME_FORMAT_PTR = UnsafeMutablePointer<uint32>
var CSSM_DB_ATTRIBUTE_NAME_AS_STRING: Int { get }
var CSSM_DB_ATTRIBUTE_NAME_AS_OID: Int { get }
var CSSM_DB_ATTRIBUTE_NAME_AS_INTEGER: Int { get }
typealias CSSM_DB_ATTRIBUTE_FORMAT = uint32
typealias CSSM_DB_ATTRIBUTE_FORMAT_PTR = UnsafeMutablePointer<uint32>
var CSSM_DB_ATTRIBUTE_FORMAT_STRING: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_SINT32: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_UINT32: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_BIG_NUM: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_REAL: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_TIME_DATE: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_BLOB: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_MULTI_UINT32: Int { get }
var CSSM_DB_ATTRIBUTE_FORMAT_COMPLEX: Int { get }
struct cssm_db_attribute_info {
  var AttributeNameFormat: CSSM_DB_ATTRIBUTE_NAME_FORMAT
  var Label: cssm_db_attribute_label
  var AttributeFormat: CSSM_DB_ATTRIBUTE_FORMAT
  init()
  init(AttributeNameFormat AttributeNameFormat: CSSM_DB_ATTRIBUTE_NAME_FORMAT, Label Label: cssm_db_attribute_label, AttributeFormat AttributeFormat: CSSM_DB_ATTRIBUTE_FORMAT)
}
struct cssm_db_attribute_label {
  var AttributeName: UnsafeMutablePointer<Int8>!
  var AttributeOID: CSSM_OID
  var AttributeID: uint32
  init(AttributeName AttributeName: UnsafeMutablePointer<Int8>!)
  init(AttributeOID AttributeOID: CSSM_OID)
  init(AttributeID AttributeID: uint32)
  init()
}
typealias CSSM_DB_ATTRIBUTE_INFO = cssm_db_attribute_info
typealias CSSM_DB_ATTRIBUTE_INFO_PTR = UnsafeMutablePointer<cssm_db_attribute_info>
struct cssm_db_attribute_data {
  var Info: CSSM_DB_ATTRIBUTE_INFO
  var NumberOfValues: uint32
  var Value: UnsafeMutablePointer<cssm_data>!
  init()
  init(Info Info: CSSM_DB_ATTRIBUTE_INFO, NumberOfValues NumberOfValues: uint32, Value Value: UnsafeMutablePointer<cssm_data>!)
}
typealias CSSM_DB_RECORDTYPE = uint32
var CSSM_DB_RECORDTYPE_SCHEMA_START: UInt32 { get }
var CSSM_DB_RECORDTYPE_SCHEMA_END: UInt32 { get }
var CSSM_DB_RECORDTYPE_OPEN_GROUP_START: UInt32 { get }
var CSSM_DB_RECORDTYPE_OPEN_GROUP_END: UInt32 { get }
var CSSM_DB_RECORDTYPE_APP_DEFINED_START: UInt32 { get }
var CSSM_DB_RECORDTYPE_APP_DEFINED_END: UInt32 { get }
var CSSM_DL_DB_SCHEMA_INFO: UInt32 { get }
var CSSM_DL_DB_SCHEMA_INDEXES: UInt32 { get }
var CSSM_DL_DB_SCHEMA_ATTRIBUTES: UInt32 { get }
var CSSM_DL_DB_SCHEMA_PARSING_MODULE: UInt32 { get }
var CSSM_DL_DB_RECORD_ANY: UInt32 { get }
var CSSM_DL_DB_RECORD_CERT: UInt32 { get }
var CSSM_DL_DB_RECORD_CRL: UInt32 { get }
var CSSM_DL_DB_RECORD_POLICY: UInt32 { get }
var CSSM_DL_DB_RECORD_GENERIC: UInt32 { get }
var CSSM_DL_DB_RECORD_PUBLIC_KEY: UInt32 { get }
var CSSM_DL_DB_RECORD_PRIVATE_KEY: UInt32 { get }
var CSSM_DL_DB_RECORD_SYMMETRIC_KEY: UInt32 { get }
var CSSM_DL_DB_RECORD_ALL_KEYS: UInt32 { get }
var CSSM_DB_CERT_USE_TRUSTED: Int { get }
var CSSM_DB_CERT_USE_SYSTEM: Int { get }
var CSSM_DB_CERT_USE_OWNER: Int { get }
var CSSM_DB_CERT_USE_REVOKED: Int { get }
var CSSM_DB_CERT_USE_SIGNING: Int { get }
var CSSM_DB_CERT_USE_PRIVACY: Int { get }
struct cssm_db_record_attribute_info {
  var DataRecordType: CSSM_DB_RECORDTYPE
  var NumberOfAttributes: uint32
  var AttributeInfo: CSSM_DB_ATTRIBUTE_INFO_PTR!
  init()
  init(DataRecordType DataRecordType: CSSM_DB_RECORDTYPE, NumberOfAttributes NumberOfAttributes: uint32, AttributeInfo AttributeInfo: CSSM_DB_ATTRIBUTE_INFO_PTR!)
}
struct cssm_db_record_attribute_data {
  var DataRecordType: CSSM_DB_RECORDTYPE
  var SemanticInformation: uint32
  var NumberOfAttributes: uint32
  var AttributeData: UnsafeMutablePointer<cssm_db_attribute_data>!
  init()
  init(DataRecordType DataRecordType: CSSM_DB_RECORDTYPE, SemanticInformation SemanticInformation: uint32, NumberOfAttributes NumberOfAttributes: uint32, AttributeData AttributeData: UnsafeMutablePointer<cssm_db_attribute_data>!)
}
struct cssm_db_parsing_module_info {
  var RecordType: CSSM_DB_RECORDTYPE
  var ModuleSubserviceUid: cssm_subservice_uid
  init()
  init(RecordType RecordType: CSSM_DB_RECORDTYPE, ModuleSubserviceUid ModuleSubserviceUid: cssm_subservice_uid)
}
typealias CSSM_DB_INDEX_TYPE = uint32
var CSSM_DB_INDEX_UNIQUE: Int { get }
var CSSM_DB_INDEX_NONUNIQUE: Int { get }
typealias CSSM_DB_INDEXED_DATA_LOCATION = uint32
var CSSM_DB_INDEX_ON_UNKNOWN: Int { get }
var CSSM_DB_INDEX_ON_ATTRIBUTE: Int { get }
var CSSM_DB_INDEX_ON_RECORD: Int { get }
struct cssm_db_index_info {
  var IndexType: CSSM_DB_INDEX_TYPE
  var IndexedDataLocation: CSSM_DB_INDEXED_DATA_LOCATION
  var Info: CSSM_DB_ATTRIBUTE_INFO
  init()
  init(IndexType IndexType: CSSM_DB_INDEX_TYPE, IndexedDataLocation IndexedDataLocation: CSSM_DB_INDEXED_DATA_LOCATION, Info Info: CSSM_DB_ATTRIBUTE_INFO)
}
struct cssm_db_unique_record {
  var RecordLocator: cssm_db_index_info
  var RecordIdentifier: cssm_data
  init()
  init(RecordLocator RecordLocator: cssm_db_index_info, RecordIdentifier RecordIdentifier: cssm_data)
}
struct cssm_db_record_index_info {
  var DataRecordType: CSSM_DB_RECORDTYPE
  var NumberOfIndexes: uint32
  var IndexInfo: UnsafeMutablePointer<cssm_db_index_info>!
  init()
  init(DataRecordType DataRecordType: CSSM_DB_RECORDTYPE, NumberOfIndexes NumberOfIndexes: uint32, IndexInfo IndexInfo: UnsafeMutablePointer<cssm_db_index_info>!)
}
typealias CSSM_DB_ACCESS_TYPE = uint32
typealias CSSM_DB_ACCESS_TYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_DB_ACCESS_READ: Int { get }
var CSSM_DB_ACCESS_WRITE: Int { get }
var CSSM_DB_ACCESS_PRIVILEGED: Int { get }
typealias CSSM_DB_MODIFY_MODE = uint32
var CSSM_DB_MODIFY_ATTRIBUTE_NONE: Int { get }
var CSSM_DB_MODIFY_ATTRIBUTE_ADD: Int { get }
var CSSM_DB_MODIFY_ATTRIBUTE_DELETE: Int { get }
var CSSM_DB_MODIFY_ATTRIBUTE_REPLACE: Int { get }
struct cssm_dbinfo {
  var NumberOfRecordTypes: uint32
  var DefaultParsingModules: UnsafeMutablePointer<cssm_db_parsing_module_info>!
  var RecordAttributeNames: UnsafeMutablePointer<cssm_db_record_attribute_info>!
  var RecordIndexes: UnsafeMutablePointer<cssm_db_record_index_info>!
  var IsLocal: CSSM_BOOL
  var AccessPath: UnsafeMutablePointer<Int8>!
  var Reserved: UnsafeMutablePointer<Void>!
  init()
  init(NumberOfRecordTypes NumberOfRecordTypes: uint32, DefaultParsingModules DefaultParsingModules: UnsafeMutablePointer<cssm_db_parsing_module_info>!, RecordAttributeNames RecordAttributeNames: UnsafeMutablePointer<cssm_db_record_attribute_info>!, RecordIndexes RecordIndexes: UnsafeMutablePointer<cssm_db_record_index_info>!, IsLocal IsLocal: CSSM_BOOL, AccessPath AccessPath: UnsafeMutablePointer<Int8>!, Reserved Reserved: UnsafeMutablePointer<Void>!)
}
typealias CSSM_DB_OPERATOR = uint32
typealias CSSM_DB_OPERATOR_PTR = UnsafeMutablePointer<uint32>
var CSSM_DB_EQUAL: Int { get }
var CSSM_DB_NOT_EQUAL: Int { get }
var CSSM_DB_LESS_THAN: Int { get }
var CSSM_DB_GREATER_THAN: Int { get }
var CSSM_DB_CONTAINS: Int { get }
var CSSM_DB_CONTAINS_INITIAL_SUBSTRING: Int { get }
var CSSM_DB_CONTAINS_FINAL_SUBSTRING: Int { get }
typealias CSSM_DB_CONJUNCTIVE = uint32
typealias CSSM_DB_CONJUNCTIVE_PTR = UnsafeMutablePointer<uint32>
var CSSM_DB_NONE: Int { get }
var CSSM_DB_AND: Int { get }
var CSSM_DB_OR: Int { get }
struct cssm_selection_predicate {
  var DbOperator: CSSM_DB_OPERATOR
  var Attribute: cssm_db_attribute_data
  init()
  init(DbOperator DbOperator: CSSM_DB_OPERATOR, Attribute Attribute: cssm_db_attribute_data)
}
var CSSM_QUERY_TIMELIMIT_NONE: Int { get }
var CSSM_QUERY_SIZELIMIT_NONE: Int { get }
struct cssm_query_limits {
  var TimeLimit: uint32
  var SizeLimit: uint32
  init()
  init(TimeLimit TimeLimit: uint32, SizeLimit SizeLimit: uint32)
}
typealias CSSM_QUERY_FLAGS = uint32
var CSSM_QUERY_RETURN_DATA: Int { get }
struct cssm_query {
  var RecordType: CSSM_DB_RECORDTYPE
  var Conjunctive: CSSM_DB_CONJUNCTIVE
  var NumSelectionPredicates: uint32
  var SelectionPredicate: UnsafeMutablePointer<cssm_selection_predicate>!
  var QueryLimits: cssm_query_limits
  var QueryFlags: CSSM_QUERY_FLAGS
  init()
  init(RecordType RecordType: CSSM_DB_RECORDTYPE, Conjunctive Conjunctive: CSSM_DB_CONJUNCTIVE, NumSelectionPredicates NumSelectionPredicates: uint32, SelectionPredicate SelectionPredicate: UnsafeMutablePointer<cssm_selection_predicate>!, QueryLimits QueryLimits: cssm_query_limits, QueryFlags QueryFlags: CSSM_QUERY_FLAGS)
}
typealias CSSM_DLTYPE = uint32
typealias CSSM_DLTYPE_PTR = UnsafeMutablePointer<uint32>
var CSSM_DL_UNKNOWN: Int { get }
var CSSM_DL_CUSTOM: Int { get }
var CSSM_DL_LDAP: Int { get }
var CSSM_DL_ODBC: Int { get }
var CSSM_DL_PKCS11: Int { get }
var CSSM_DL_FFS: Int { get }
var CSSM_DL_MEMORY: Int { get }
var CSSM_DL_REMOTEDIR: Int { get }
typealias CSSM_DL_CUSTOM_ATTRIBUTES = UnsafeMutablePointer<Void>
typealias CSSM_DL_LDAP_ATTRIBUTES = UnsafeMutablePointer<Void>
typealias CSSM_DL_ODBC_ATTRIBUTES = UnsafeMutablePointer<Void>
typealias CSSM_DL_FFS_ATTRIBUTES = UnsafeMutablePointer<Void>
struct cssm_dl_pkcs11_attributes {
  var DeviceAccessFlags: uint32
  init()
  init(DeviceAccessFlags DeviceAccessFlags: uint32)
}
typealias CSSM_DL_PKCS11_ATTRIBUTE = UnsafeMutablePointer<cssm_dl_pkcs11_attributes>
typealias CSSM_DL_PKCS11_ATTRIBUTE_PTR = UnsafeMutablePointer<cssm_dl_pkcs11_attributes>
var CSSM_DB_DATASTORES_UNKNOWN: UInt32 { get }
struct cssm_name_list {
  var NumStrings: uint32
  var String: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!
  init()
  init(NumStrings NumStrings: uint32, String String: UnsafeMutablePointer<UnsafeMutablePointer<Int8>?>!)
}
typealias CSSM_DB_RETRIEVAL_MODES = uint32
var CSSM_DB_TRANSACTIONAL_MODE: Int { get }
var CSSM_DB_FILESYSTEMSCAN_MODE: Int { get }
struct cssm_db_schema_attribute_info {
  var AttributeId: uint32
  var AttributeName: UnsafeMutablePointer<Int8>!
  var AttributeNameID: CSSM_OID
  var DataType: CSSM_DB_ATTRIBUTE_FORMAT
  init()
  init(AttributeId AttributeId: uint32, AttributeName AttributeName: UnsafeMutablePointer<Int8>!, AttributeNameID AttributeNameID: CSSM_OID, DataType DataType: CSSM_DB_ATTRIBUTE_FORMAT)
}
struct cssm_db_schema_index_info {
  var AttributeId: uint32
  var IndexId: uint32
  var IndexType: CSSM_DB_INDEX_TYPE
  var IndexedDataLocation: CSSM_DB_INDEXED_DATA_LOCATION
  init()
  init(AttributeId AttributeId: uint32, IndexId IndexId: uint32, IndexType IndexType: CSSM_DB_INDEX_TYPE, IndexedDataLocation IndexedDataLocation: CSSM_DB_INDEXED_DATA_LOCATION)
}
