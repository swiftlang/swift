
typealias SecAsn1Item = cssm_data
typealias SecAsn1Oid = CSSM_OID
typealias SecAsn1AlgId = cssm_x509_algorithm_identifier
typealias SecAsn1PubKeyInfo = cssm_x509_subject_public_key_info
struct SecAsn1Template_struct {
  var kind: UInt32
  var offset: UInt32
  var sub: UnsafePointer<Void>
  var size: UInt32
}
typealias SecAsn1Template = SecAsn1Template_struct
var SEC_ASN1_TAG_MASK: Int32 { get }
var SEC_ASN1_TAGNUM_MASK: Int32 { get }
var SEC_ASN1_BOOLEAN: Int32 { get }
var SEC_ASN1_INTEGER: Int32 { get }
var SEC_ASN1_BIT_STRING: Int32 { get }
var SEC_ASN1_OCTET_STRING: Int32 { get }
var SEC_ASN1_NULL: Int32 { get }
var SEC_ASN1_OBJECT_ID: Int32 { get }
var SEC_ASN1_OBJECT_DESCRIPTOR: Int32 { get }
var SEC_ASN1_REAL: Int32 { get }
var SEC_ASN1_ENUMERATED: Int32 { get }
var SEC_ASN1_EMBEDDED_PDV: Int32 { get }
var SEC_ASN1_UTF8_STRING: Int32 { get }
var SEC_ASN1_SEQUENCE: Int32 { get }
var SEC_ASN1_SET: Int32 { get }
var SEC_ASN1_NUMERIC_STRING: Int32 { get }
var SEC_ASN1_PRINTABLE_STRING: Int32 { get }
var SEC_ASN1_T61_STRING: Int32 { get }
var SEC_ASN1_VIDEOTEX_STRING: Int32 { get }
var SEC_ASN1_IA5_STRING: Int32 { get }
var SEC_ASN1_UTC_TIME: Int32 { get }
var SEC_ASN1_GENERALIZED_TIME: Int32 { get }
var SEC_ASN1_GRAPHIC_STRING: Int32 { get }
var SEC_ASN1_VISIBLE_STRING: Int32 { get }
var SEC_ASN1_GENERAL_STRING: Int32 { get }
var SEC_ASN1_UNIVERSAL_STRING: Int32 { get }
var SEC_ASN1_BMP_STRING: Int32 { get }
var SEC_ASN1_HIGH_TAG_NUMBER: Int32 { get }
var SEC_ASN1_TELETEX_STRING: Int32 { get }
var SEC_ASN1_METHOD_MASK: Int32 { get }
var SEC_ASN1_PRIMITIVE: Int32 { get }
var SEC_ASN1_CONSTRUCTED: Int32 { get }
var SEC_ASN1_CLASS_MASK: Int32 { get }
var SEC_ASN1_UNIVERSAL: Int32 { get }
var SEC_ASN1_APPLICATION: Int32 { get }
var SEC_ASN1_CONTEXT_SPECIFIC: Int32 { get }
var SEC_ASN1_PRIVATE: Int32 { get }
var SEC_ASN1_OPTIONAL: Int32 { get }
var SEC_ASN1_EXPLICIT: Int32 { get }
var SEC_ASN1_ANY: Int32 { get }
var SEC_ASN1_INLINE: Int32 { get }
var SEC_ASN1_POINTER: Int32 { get }
var SEC_ASN1_GROUP: Int32 { get }
var SEC_ASN1_DYNAMIC: Int32 { get }
var SEC_ASN1_SKIP: Int32 { get }
var SEC_ASN1_INNER: Int32 { get }
var SEC_ASN1_SAVE: Int32 { get }
var SEC_ASN1_SKIP_REST: Int32 { get }
var SEC_ASN1_CHOICE: Int32 { get }
var SEC_ASN1_SIGNED_INT: Int32 { get }
typealias SecAsn1TemplateChooser = (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafePointer<Int8>, Int, UnsafeMutablePointer<Void>) -> UnsafePointer<SecAsn1Template>!
typealias SecAsn1TemplateChooserPtr = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafePointer<Int8>, Int, UnsafeMutablePointer<Void>) -> UnsafePointer<SecAsn1Template>!
