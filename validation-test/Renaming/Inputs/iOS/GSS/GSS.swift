
typealias OM_uint32 = UInt32
typealias OM_uint64 = UInt64
typealias gss_uint32 = UInt32
typealias gss_name_t = OpaquePointer
typealias gss_const_name_t = OpaquePointer
typealias gss_ctx_id_t = OpaquePointer
struct gss_OID_desc_struct {
  var length: OM_uint32
  var elements: UnsafeMutablePointer<Void>!
  init()
  init(length length: OM_uint32, elements elements: UnsafeMutablePointer<Void>!)
}
typealias gss_OID_desc = gss_OID_desc_struct
typealias gss_OID = UnsafeMutablePointer<gss_OID_desc_struct>
typealias gss_const_OID = UnsafePointer<gss_OID_desc>
struct gss_OID_set_desc_struct {
  var count: Int
  var elements: gss_OID!
  init()
  init(count count: Int, elements elements: gss_OID!)
}
typealias gss_OID_set_desc = gss_OID_set_desc_struct
typealias gss_OID_set = UnsafeMutablePointer<gss_OID_set_desc_struct>
typealias gss_const_OID_set = UnsafePointer<gss_OID_set_desc>
typealias gss_cred_usage_t = Int32
typealias gss_cred_id_t = OpaquePointer
typealias gss_const_cred_id_t = OpaquePointer
struct gss_buffer_desc_struct {
  var length: Int
  var value: UnsafeMutablePointer<Void>!
  init()
  init(length length: Int, value value: UnsafeMutablePointer<Void>!)
}
typealias gss_buffer_desc = gss_buffer_desc_struct
typealias gss_buffer_t = UnsafeMutablePointer<gss_buffer_desc_struct>
typealias gss_const_buffer_t = UnsafePointer<gss_buffer_desc>
struct gss_channel_bindings_struct {
  var initiator_addrtype: OM_uint32
  var initiator_address: gss_buffer_desc
  var acceptor_addrtype: OM_uint32
  var acceptor_address: gss_buffer_desc
  var application_data: gss_buffer_desc
  init()
  init(initiator_addrtype initiator_addrtype: OM_uint32, initiator_address initiator_address: gss_buffer_desc, acceptor_addrtype acceptor_addrtype: OM_uint32, acceptor_address acceptor_address: gss_buffer_desc, application_data application_data: gss_buffer_desc)
}
typealias gss_channel_bindings_t = UnsafeMutablePointer<gss_channel_bindings_struct>
typealias gss_const_channel_bindings_t = UnsafePointer<gss_channel_bindings_struct>
struct gss_buffer_set_desc_struct {
  var count: Int
  var elements: UnsafeMutablePointer<gss_buffer_desc>!
  init()
  init(count count: Int, elements elements: UnsafeMutablePointer<gss_buffer_desc>!)
}
typealias gss_buffer_set_desc = gss_buffer_set_desc_struct
typealias gss_buffer_set_t = UnsafeMutablePointer<gss_buffer_set_desc_struct>
struct gss_iov_buffer_desc_struct {
  var type: OM_uint32
  var buffer: gss_buffer_desc
  init()
  init(type type: OM_uint32, buffer buffer: gss_buffer_desc)
}
typealias gss_iov_buffer_desc = gss_iov_buffer_desc_struct
typealias gss_iov_buffer_t = UnsafeMutablePointer<gss_iov_buffer_desc_struct>
typealias gss_qop_t = OM_uint32
typealias gss_status_id_t = UnsafeMutablePointer<OM_uint32>
typealias gss_auth_identity_t = OpaquePointer
var GSS_C_DELEG_FLAG: Int32 { get }
var GSS_C_MUTUAL_FLAG: Int32 { get }
var GSS_C_REPLAY_FLAG: Int32 { get }
var GSS_C_SEQUENCE_FLAG: Int32 { get }
var GSS_C_CONF_FLAG: Int32 { get }
var GSS_C_INTEG_FLAG: Int32 { get }
var GSS_C_ANON_FLAG: Int32 { get }
var GSS_C_PROT_READY_FLAG: Int32 { get }
var GSS_C_TRANS_FLAG: Int32 { get }
var GSS_C_DCE_STYLE: Int32 { get }
var GSS_C_IDENTIFY_FLAG: Int32 { get }
var GSS_C_EXTENDED_ERROR_FLAG: Int32 { get }
var GSS_C_DELEG_POLICY_FLAG: Int32 { get }
var GSS_C_BOTH: Int32 { get }
var GSS_C_INITIATE: Int32 { get }
var GSS_C_ACCEPT: Int32 { get }
var GSS_C_GSS_CODE: Int32 { get }
var GSS_C_MECH_CODE: Int32 { get }
var GSS_C_AF_UNSPEC: Int32 { get }
var GSS_C_AF_LOCAL: Int32 { get }
var GSS_C_AF_INET: Int32 { get }
var GSS_C_AF_IMPLINK: Int32 { get }
var GSS_C_AF_PUP: Int32 { get }
var GSS_C_AF_CHAOS: Int32 { get }
var GSS_C_AF_NS: Int32 { get }
var GSS_C_AF_NBS: Int32 { get }
var GSS_C_AF_ECMA: Int32 { get }
var GSS_C_AF_DATAKIT: Int32 { get }
var GSS_C_AF_CCITT: Int32 { get }
var GSS_C_AF_SNA: Int32 { get }
var GSS_C_AF_DECnet: Int32 { get }
var GSS_C_AF_DLI: Int32 { get }
var GSS_C_AF_LAT: Int32 { get }
var GSS_C_AF_HYLINK: Int32 { get }
var GSS_C_AF_APPLETALK: Int32 { get }
var GSS_C_AF_BSC: Int32 { get }
var GSS_C_AF_DSS: Int32 { get }
var GSS_C_AF_OSI: Int32 { get }
var GSS_C_AF_X25: Int32 { get }
var GSS_C_AF_INET6: Int32 { get }
var GSS_C_AF_NULLADDR: Int32 { get }
var GSS_C_QOP_DEFAULT: Int32 { get }
var GSS_KRB5_CONF_C_QOP_DES: Int32 { get }
var GSS_KRB5_CONF_C_QOP_DES3_KD: Int32 { get }
var GSS_C_INDEFINITE: UInt { get }
var GSS_IOV_BUFFER_TYPE_EMPTY: Int32 { get }
var GSS_IOV_BUFFER_TYPE_DATA: Int32 { get }
var GSS_IOV_BUFFER_TYPE_HEADER: Int32 { get }
var GSS_IOV_BUFFER_TYPE_MECH_PARAMS: Int32 { get }
var GSS_IOV_BUFFER_TYPE_TRAILER: Int32 { get }
var GSS_IOV_BUFFER_TYPE_PADDING: Int32 { get }
var GSS_IOV_BUFFER_TYPE_STREAM: Int32 { get }
var GSS_IOV_BUFFER_TYPE_SIGN_ONLY: Int32 { get }
var GSS_IOV_BUFFER_TYPE_FLAG_MASK: UInt32 { get }
var GSS_IOV_BUFFER_FLAG_ALLOCATE: Int32 { get }
var GSS_IOV_BUFFER_FLAG_ALLOCATED: Int32 { get }
var GSS_IOV_BUFFER_TYPE_FLAG_ALLOCATE: Int32 { get }
var GSS_IOV_BUFFER_TYPE_FLAG_ALLOCATED: Int32 { get }
var GSS_S_COMPLETE: Int32 { get }
var GSS_C_CALLING_ERROR_OFFSET: Int32 { get }
var GSS_C_ROUTINE_ERROR_OFFSET: Int32 { get }
var GSS_C_SUPPLEMENTARY_OFFSET: Int32 { get }
var GSS_C_CALLING_ERROR_MASK: UInt { get }
var GSS_C_ROUTINE_ERROR_MASK: UInt { get }
var GSS_C_SUPPLEMENTARY_MASK: UInt { get }
var GSS_C_OPTION_MASK: Int32 { get }
var GSS_C_CRED_NO_UI: Int32 { get }
var GSS_C_PRF_KEY_FULL: Int32 { get }
var GSS_C_PRF_KEY_PARTIAL: Int32 { get }
var __gss_c_attr_local_login_user: gss_buffer_desc
var kGSSICPassword: String { get }
var kGSSICCertificate: String { get }
var kGSSICVerifyCredential: String { get }
var kGSSCredentialUsage: String { get }
var kGSS_C_INITIATE: String { get }
var kGSS_C_ACCEPT: String { get }
var kGSS_C_BOTH: String { get }
var kGSSICLKDCHostname: String { get }
var kGSSICKerberosCacheName: String { get }
var kGSSICSiteName: String { get }
var kGSSICAppIdentifierACL: String { get }
var kGSSICVerifyCredentialAcceptorName: String { get }
var kGSSICCreateNewCredential: String { get }
var kGSSICAppleSourceApp: String { get }
var kGSSICAppleSourceAppAuditToken: String { get }
var kGSSICAppleSourceAppPID: String { get }
var kGSSICAppleSourceAppSigningIdentity: String { get }
var kGSSChangePasswordOldPassword: String { get }
var kGSSChangePasswordNewPassword: String { get }
