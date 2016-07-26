
typealias CSSM_BER_TAG = uint8
var BER_TAG_UNKNOWN: Int32 { get }
var BER_TAG_BOOLEAN: Int32 { get }
var BER_TAG_INTEGER: Int32 { get }
var BER_TAG_BIT_STRING: Int32 { get }
var BER_TAG_OCTET_STRING: Int32 { get }
var BER_TAG_NULL: Int32 { get }
var BER_TAG_OID: Int32 { get }
var BER_TAG_OBJECT_DESCRIPTOR: Int32 { get }
var BER_TAG_EXTERNAL: Int32 { get }
var BER_TAG_REAL: Int32 { get }
var BER_TAG_ENUMERATED: Int32 { get }
var BER_TAG_PKIX_UTF8_STRING: Int32 { get }
var BER_TAG_SEQUENCE: Int32 { get }
var BER_TAG_SET: Int32 { get }
var BER_TAG_NUMERIC_STRING: Int32 { get }
var BER_TAG_PRINTABLE_STRING: Int32 { get }
var BER_TAG_T61_STRING: Int32 { get }
var BER_TAG_TELETEX_STRING: Int32 { get }
var BER_TAG_VIDEOTEX_STRING: Int32 { get }
var BER_TAG_IA5_STRING: Int32 { get }
var BER_TAG_UTC_TIME: Int32 { get }
var BER_TAG_GENERALIZED_TIME: Int32 { get }
var BER_TAG_GRAPHIC_STRING: Int32 { get }
var BER_TAG_ISO646_STRING: Int32 { get }
var BER_TAG_GENERAL_STRING: Int32 { get }
var BER_TAG_VISIBLE_STRING: Int32 { get }
var BER_TAG_PKIX_UNIVERSAL_STRING: Int32 { get }
var BER_TAG_PKIX_BMP_STRING: Int32 { get }
struct cssm_x509_algorithm_identifier {
  var algorithm: CSSM_OID
  var parameters: cssm_data
  init()
  init(algorithm algorithm: CSSM_OID, parameters parameters: cssm_data)
}
struct cssm_x509_type_value_pair {
  var type: CSSM_OID
  var valueType: CSSM_BER_TAG
  var value: cssm_data
  init()
  init(type type: CSSM_OID, valueType valueType: CSSM_BER_TAG, value value: cssm_data)
}
struct cssm_x509_rdn {
  var numberOfPairs: uint32
  var AttributeTypeAndValue: UnsafeMutablePointer<cssm_x509_type_value_pair>!
  init()
  init(numberOfPairs numberOfPairs: uint32, AttributeTypeAndValue AttributeTypeAndValue: UnsafeMutablePointer<cssm_x509_type_value_pair>!)
}
struct cssm_x509_name {
  var numberOfRDNs: uint32
  var RelativeDistinguishedName: UnsafeMutablePointer<cssm_x509_rdn>!
  init()
  init(numberOfRDNs numberOfRDNs: uint32, RelativeDistinguishedName RelativeDistinguishedName: UnsafeMutablePointer<cssm_x509_rdn>!)
}
struct cssm_x509_subject_public_key_info {
  var algorithm: cssm_x509_algorithm_identifier
  var subjectPublicKey: cssm_data
  init()
  init(algorithm algorithm: cssm_x509_algorithm_identifier, subjectPublicKey subjectPublicKey: cssm_data)
}
struct cssm_x509_time {
  var timeType: CSSM_BER_TAG
  var time: cssm_data
  init()
  init(timeType timeType: CSSM_BER_TAG, time time: cssm_data)
}
struct x509_validity {
  var notBefore: cssm_x509_time
  var notAfter: cssm_x509_time
  init()
  init(notBefore notBefore: cssm_x509_time, notAfter notAfter: cssm_x509_time)
}
typealias CSSM_X509_OPTION = CSSM_BOOL
struct cssm_x509ext_basicConstraints {
  var cA: CSSM_BOOL
  var pathLenConstraintPresent: CSSM_X509_OPTION
  var pathLenConstraint: uint32
  init()
  init(cA cA: CSSM_BOOL, pathLenConstraintPresent pathLenConstraintPresent: CSSM_X509_OPTION, pathLenConstraint pathLenConstraint: uint32)
}
struct extension_data_format : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var CSSM_X509_DATAFORMAT_ENCODED: extension_data_format { get }
var CSSM_X509_DATAFORMAT_PARSED: extension_data_format { get }
var CSSM_X509_DATAFORMAT_PAIR: extension_data_format { get }
typealias CSSM_X509EXT_DATA_FORMAT = extension_data_format
struct cssm_x509_extensionTagAndValue {
  var type: CSSM_BER_TAG
  var value: cssm_data
  init()
  init(type type: CSSM_BER_TAG, value value: cssm_data)
}
struct cssm_x509ext_pair {
  var tagAndValue: cssm_x509_extensionTagAndValue
  var parsedValue: UnsafeMutablePointer<Void>!
  init()
  init(tagAndValue tagAndValue: cssm_x509_extensionTagAndValue, parsedValue parsedValue: UnsafeMutablePointer<Void>!)
}
struct cssm_x509_extension {
  var extnId: CSSM_OID
  var critical: CSSM_BOOL
  var format: CSSM_X509EXT_DATA_FORMAT
  var value: cssm_x509ext_value
  var BERvalue: cssm_data
  init()
  init(extnId extnId: CSSM_OID, critical critical: CSSM_BOOL, format format: CSSM_X509EXT_DATA_FORMAT, value value: cssm_x509ext_value, BERvalue BERvalue: cssm_data)
}
struct cssm_x509ext_value {
  var tagAndValue: UnsafeMutablePointer<cssm_x509_extensionTagAndValue>!
  var parsedValue: UnsafeMutablePointer<Void>!
  var valuePair: UnsafeMutablePointer<cssm_x509ext_pair>!
  init(tagAndValue tagAndValue: UnsafeMutablePointer<cssm_x509_extensionTagAndValue>!)
  init(parsedValue parsedValue: UnsafeMutablePointer<Void>!)
  init(valuePair valuePair: UnsafeMutablePointer<cssm_x509ext_pair>!)
  init()
}
struct cssm_x509_extensions {
  var numberOfExtensions: uint32
  var extensions: UnsafeMutablePointer<cssm_x509_extension>!
  init()
  init(numberOfExtensions numberOfExtensions: uint32, extensions extensions: UnsafeMutablePointer<cssm_x509_extension>!)
}
struct cssm_x509_tbs_certificate {
  var version: cssm_data
  var serialNumber: cssm_data
  var signature: cssm_x509_algorithm_identifier
  var issuer: cssm_x509_name
  var validity: x509_validity
  var subject: cssm_x509_name
  var subjectPublicKeyInfo: cssm_x509_subject_public_key_info
  var issuerUniqueIdentifier: cssm_data
  var subjectUniqueIdentifier: cssm_data
  var extensions: cssm_x509_extensions
  init()
  init(version version: cssm_data, serialNumber serialNumber: cssm_data, signature signature: cssm_x509_algorithm_identifier, issuer issuer: cssm_x509_name, validity validity: x509_validity, subject subject: cssm_x509_name, subjectPublicKeyInfo subjectPublicKeyInfo: cssm_x509_subject_public_key_info, issuerUniqueIdentifier issuerUniqueIdentifier: cssm_data, subjectUniqueIdentifier subjectUniqueIdentifier: cssm_data, extensions extensions: cssm_x509_extensions)
}
struct cssm_x509_signature {
  var algorithmIdentifier: cssm_x509_algorithm_identifier
  var encrypted: cssm_data
  init()
  init(algorithmIdentifier algorithmIdentifier: cssm_x509_algorithm_identifier, encrypted encrypted: cssm_data)
}
struct cssm_x509_signed_certificate {
  var certificate: cssm_x509_tbs_certificate
  var signature: cssm_x509_signature
  init()
  init(certificate certificate: cssm_x509_tbs_certificate, signature signature: cssm_x509_signature)
}
struct cssm_x509ext_policyQualifierInfo {
  var policyQualifierId: CSSM_OID
  var value: cssm_data
  init()
  init(policyQualifierId policyQualifierId: CSSM_OID, value value: cssm_data)
}
struct cssm_x509ext_policyQualifiers {
  var numberOfPolicyQualifiers: uint32
  var policyQualifier: UnsafeMutablePointer<cssm_x509ext_policyQualifierInfo>!
  init()
  init(numberOfPolicyQualifiers numberOfPolicyQualifiers: uint32, policyQualifier policyQualifier: UnsafeMutablePointer<cssm_x509ext_policyQualifierInfo>!)
}
struct cssm_x509ext_policyInfo {
  var policyIdentifier: CSSM_OID
  var policyQualifiers: cssm_x509ext_policyQualifiers
  init()
  init(policyIdentifier policyIdentifier: CSSM_OID, policyQualifiers policyQualifiers: cssm_x509ext_policyQualifiers)
}
struct cssm_x509_revoked_cert_entry {
  var certificateSerialNumber: cssm_data
  var revocationDate: cssm_x509_time
  var extensions: cssm_x509_extensions
  init()
  init(certificateSerialNumber certificateSerialNumber: cssm_data, revocationDate revocationDate: cssm_x509_time, extensions extensions: cssm_x509_extensions)
}
struct cssm_x509_revoked_cert_list {
  var numberOfRevokedCertEntries: uint32
  var revokedCertEntry: UnsafeMutablePointer<cssm_x509_revoked_cert_entry>!
  init()
  init(numberOfRevokedCertEntries numberOfRevokedCertEntries: uint32, revokedCertEntry revokedCertEntry: UnsafeMutablePointer<cssm_x509_revoked_cert_entry>!)
}
struct cssm_x509_tbs_certlist {
  var version: cssm_data
  var signature: cssm_x509_algorithm_identifier
  var issuer: cssm_x509_name
  var thisUpdate: cssm_x509_time
  var nextUpdate: cssm_x509_time
  var revokedCertificates: UnsafeMutablePointer<cssm_x509_revoked_cert_list>!
  var extensions: cssm_x509_extensions
  init()
  init(version version: cssm_data, signature signature: cssm_x509_algorithm_identifier, issuer issuer: cssm_x509_name, thisUpdate thisUpdate: cssm_x509_time, nextUpdate nextUpdate: cssm_x509_time, revokedCertificates revokedCertificates: UnsafeMutablePointer<cssm_x509_revoked_cert_list>!, extensions extensions: cssm_x509_extensions)
}
struct cssm_x509_signed_crl {
  var tbsCertList: cssm_x509_tbs_certlist
  var signature: cssm_x509_signature
  init()
  init(tbsCertList tbsCertList: cssm_x509_tbs_certlist, signature signature: cssm_x509_signature)
}
