
@available(OSX 10.9, *)
@discardableResult
func GSSCreateCredentialFromUUID(_ uuid: CFUUID) -> gss_cred_id_t?
@available(OSX 10.10, *)
@discardableResult
func GSSCreateError(_ mech: gss_const_OID, _ major_status: OM_uint32, _ minor_status: OM_uint32) -> Unmanaged<CFError>?
@available(OSX 10.9, *)
@discardableResult
func GSSCreateName(_ name: CFTypeRef, _ name_type: gss_const_OID, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> gss_name_t?
@available(OSX 10.9, *)
@discardableResult
func GSSCredentialCopyName(_ cred: gss_cred_id_t) -> gss_name_t?
@available(OSX 10.9, *)
@discardableResult
func GSSCredentialCopyUUID(_ credential: gss_cred_id_t) -> Unmanaged<CFUUID>?
@available(OSX 10.9, *)
@discardableResult
func GSSCredentialGetLifetime(_ cred: gss_cred_id_t) -> OM_uint32
@available(OSX 10.9, *)
@discardableResult
func GSSNameCreateDisplayString(_ name: gss_name_t) -> Unmanaged<CFString>?
@available(OSX 10.9, *)
@discardableResult
func gss_aapl_change_password(_ name: gss_name_t, _ mech: gss_const_OID, _ attributes: CFDictionary, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_aapl_initial_cred(_ desired_name: gss_name_t, _ desired_mech: gss_const_OID, _ attributes: CFDictionary?, _ output_cred_handle: UnsafeMutablePointer<gss_cred_id_t>?, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> OM_uint32
