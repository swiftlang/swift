
@available(OSX 10.7, *)
@discardableResult
func gss_accept_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ acceptor_cred_handle: gss_cred_id_t?, _ input_token: gss_buffer_t?, _ input_chan_bindings: gss_channel_bindings_t?, _ src_name: UnsafeMutablePointer<gss_name_t?>?, _ mech_type: UnsafeMutablePointer<gss_OID?>?, _ output_token: gss_buffer_t, _ ret_flags: UnsafeMutablePointer<OM_uint32>?, _ time_rec: UnsafeMutablePointer<OM_uint32>?, _ delegated_cred_handle: UnsafeMutablePointer<gss_cred_id_t?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_acquire_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ desired_name: gss_name_t?, _ time_req: OM_uint32, _ desired_mechs: gss_OID_set?, _ cred_usage: gss_cred_usage_t, _ output_cred_handle: UnsafeMutablePointer<gss_cred_id_t>?, _ actual_mechs: UnsafeMutablePointer<gss_OID_set?>?, _ time_rec: UnsafeMutablePointer<OM_uint32>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_acquire_cred_with_password(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ desired_name: gss_name_t, _ password: gss_buffer_t, _ time_req: OM_uint32, _ desired_mechs: gss_OID_set?, _ cred_usage: gss_cred_usage_t, _ output_cred_handle: UnsafeMutablePointer<gss_cred_id_t>?, _ actual_mechs: UnsafeMutablePointer<gss_OID_set?>?, _ time_rec: UnsafeMutablePointer<OM_uint32>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_add_buffer_set_member(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ member_buffer: gss_buffer_t, _ buffer_set: UnsafeMutablePointer<gss_buffer_set_t>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_add_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_cred_handle: gss_cred_id_t?, _ desired_name: gss_name_t?, _ desired_mech: gss_OID?, _ cred_usage: gss_cred_usage_t, _ initiator_time_req: OM_uint32, _ acceptor_time_req: OM_uint32, _ output_cred_handle: UnsafeMutablePointer<gss_cred_id_t>?, _ actual_mechs: UnsafeMutablePointer<gss_OID_set?>?, _ initiator_time_rec: UnsafeMutablePointer<OM_uint32>?, _ acceptor_time_rec: UnsafeMutablePointer<OM_uint32>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_add_oid_set_member(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ member_oid: gss_const_OID, _ oid_set: UnsafeMutablePointer<gss_OID_set>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_canonicalize_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: gss_name_t, _ mech_type: gss_OID, _ output_name: UnsafeMutablePointer<gss_name_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_compare_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ name1_arg: gss_name_t, _ name2_arg: gss_name_t, _ name_equal: UnsafeMutablePointer<Int32>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_context_time(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ time_rec: UnsafeMutablePointer<OM_uint32>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_create_empty_buffer_set(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ buffer_set: UnsafeMutablePointer<gss_buffer_set_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_create_empty_oid_set(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ oid_set: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_decapsulate_token(_ input_token: gss_const_buffer_t, _ oid: gss_const_OID, _ output_token: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_delete_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ output_token: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_destroy_cred(_ min_stat: UnsafeMutablePointer<OM_uint32>, _ cred_handle: UnsafeMutablePointer<gss_cred_id_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_display_mech_attr(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ mech_attr: gss_const_OID, _ name: gss_buffer_t?, _ short_desc: gss_buffer_t?, _ long_desc: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_display_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: gss_name_t, _ output_name_buffer: gss_buffer_t, _ output_name_type: UnsafeMutablePointer<gss_OID?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_display_status(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ status_value: OM_uint32, _ status_type: Int32, _ mech_type: gss_OID?, _ message_content: UnsafeMutablePointer<OM_uint32>, _ status_string: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_duplicate_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ src_name: gss_name_t, _ dest_name: UnsafeMutablePointer<gss_name_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_encapsulate_token(_ input_token: gss_const_buffer_t, _ oid: gss_const_OID, _ output_token: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_export_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: gss_cred_id_t, _ token: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_export_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: gss_name_t, _ exported_name: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_export_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ interprocess_token: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_get_mic(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ qop_req: gss_qop_t, _ message_buffer: gss_buffer_t, _ message_token: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_import_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ token: gss_buffer_t, _ cred_handle: UnsafeMutablePointer<gss_cred_id_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_import_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name_buffer: gss_buffer_t, _ input_name_type: gss_const_OID?, _ output_name: UnsafeMutablePointer<gss_name_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_import_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ interprocess_token: gss_buffer_t, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_indicate_mechs(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ mech_set: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.10, *)
@discardableResult
func gss_indicate_mechs_by_attrs(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ desired_mech_attrs: gss_const_OID_set?, _ except_mech_attrs: gss_const_OID_set?, _ critical_mech_attrs: gss_const_OID_set?, _ mechs: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_init_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ initiator_cred_handle: gss_cred_id_t?, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ target_name: gss_name_t, _ input_mech_type: gss_OID?, _ req_flags: OM_uint32, _ time_req: OM_uint32, _ input_chan_bindings: gss_channel_bindings_t?, _ input_token: gss_buffer_t?, _ actual_mech_type: UnsafeMutablePointer<gss_OID?>?, _ output_token: gss_buffer_t, _ ret_flags: UnsafeMutablePointer<OM_uint32>?, _ time_rec: UnsafeMutablePointer<OM_uint32>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_attrs_for_mech(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ mech: gss_const_OID, _ mech_attr: UnsafeMutablePointer<gss_OID_set?>?, _ known_mech_attrs: UnsafeMutablePointer<gss_OID_set?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ src_name: UnsafeMutablePointer<gss_name_t?>?, _ targ_name: UnsafeMutablePointer<gss_name_t?>?, _ lifetime_rec: UnsafeMutablePointer<OM_uint32>?, _ mech_type: UnsafeMutablePointer<gss_OID?>?, _ ctx_flags: UnsafeMutablePointer<OM_uint32>?, _ locally_initiated: UnsafeMutablePointer<Int32>?, _ xopen: UnsafeMutablePointer<Int32>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: gss_cred_id_t?, _ name_ret: UnsafeMutablePointer<gss_name_t?>?, _ lifetime: UnsafeMutablePointer<OM_uint32>?, _ cred_usage: UnsafeMutablePointer<gss_cred_usage_t>?, _ mechanisms: UnsafeMutablePointer<gss_OID_set?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_cred_by_mech(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: gss_cred_id_t?, _ mech_type: gss_OID, _ cred_name: UnsafeMutablePointer<gss_name_t?>?, _ initiator_lifetime: UnsafeMutablePointer<OM_uint32>?, _ acceptor_lifetime: UnsafeMutablePointer<OM_uint32>?, _ cred_usage: UnsafeMutablePointer<gss_cred_usage_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_cred_by_oid(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: gss_cred_id_t, _ desired_object: gss_OID, _ data_set: UnsafeMutablePointer<gss_buffer_set_t>?) -> OM_uint32
@available(OSX 10.10, *)
@discardableResult
func gss_inquire_mech_for_saslname(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ sasl_mech_name: gss_buffer_t?, _ mech_type: UnsafeMutablePointer<gss_OID?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_mechs_for_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: gss_name_t, _ mech_types: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: gss_name_t, _ name_is_MN: UnsafeMutablePointer<Int32>, _ MN_mech: UnsafeMutablePointer<gss_OID?>?, _ attrs: UnsafeMutablePointer<gss_buffer_set_t?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_names_for_mech(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ mechanism: gss_const_OID, _ name_types: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.10, *)
@discardableResult
func gss_inquire_saslname_for_mech(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ desired_mech: gss_OID, _ sasl_mech_name: gss_buffer_t?, _ mech_name: gss_buffer_t?, _ mech_description: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_inquire_sec_context_by_oid(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ desired_object: gss_OID, _ data_set: UnsafeMutablePointer<gss_buffer_set_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_iter_creds(_ min_stat: UnsafeMutablePointer<OM_uint32>, _ flags: OM_uint32, _ mech: gss_const_OID?, _ useriter: (gss_OID?, gss_cred_id_t?) -> Void) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_iter_creds_f(_ min_stat: UnsafeMutablePointer<OM_uint32>, _ flags: OM_uint32, _ mech: gss_const_OID?, _ userctx: UnsafeMutablePointer<Void>?, _ useriter: @convention(c) (UnsafeMutablePointer<Void>?, gss_OID?, gss_cred_id_t?) -> Void) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_krb5_ccache_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ name: UnsafePointer<Int8>?, _ out_name: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_krb5_export_lucid_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ version: OM_uint32, _ rctx: UnsafeMutablePointer<UnsafeMutablePointer<Void>>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_krb5_free_lucid_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ c: UnsafeMutablePointer<Void>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_krb5_set_allowable_enctypes(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred: gss_cred_id_t, _ num_enctypes: OM_uint32, _ enctypes: UnsafeMutablePointer<Int32>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_oid_equal(_ a: gss_const_OID?, _ b: gss_const_OID?) -> Int32
@available(OSX 10.7, *)
@discardableResult
func gss_oid_to_str(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ oid: gss_OID, _ oid_str: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_process_context_token(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ token_buffer: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_pseudo_random(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context: gss_ctx_id_t, _ prf_key: Int32, _ prf_in: gss_buffer_t, _ desired_output_len: Int, _ prf_out: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_release_buffer(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ buffer: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_release_buffer_set(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ buffer_set: UnsafeMutablePointer<gss_buffer_set_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_release_cred(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: UnsafeMutablePointer<gss_cred_id_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_release_name(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ input_name: UnsafeMutablePointer<gss_name_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_release_oid_set(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ set: UnsafeMutablePointer<gss_OID_set>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_set_cred_option(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ cred_handle: UnsafeMutablePointer<gss_cred_id_t?>?, _ object: gss_OID, _ value: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_set_sec_context_option(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: UnsafeMutablePointer<gss_ctx_id_t>?, _ object: gss_OID, _ value: gss_buffer_t?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_test_oid_set_member(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ member: gss_const_OID, _ set: gss_OID_set, _ present: UnsafeMutablePointer<Int32>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_unwrap(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ input_message_buffer: gss_buffer_t, _ output_message_buffer: gss_buffer_t, _ conf_state: UnsafeMutablePointer<Int32>?, _ qop_state: UnsafeMutablePointer<gss_qop_t>?) -> OM_uint32
@available(OSX 10.9, *)
@discardableResult
func gss_userok(_ name: gss_name_t, _ user: UnsafePointer<Int8>) -> Int32
@available(OSX 10.7, *)
@discardableResult
func gss_verify_mic(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ message_buffer: gss_buffer_t, _ token_buffer: gss_buffer_t, _ qop_state: UnsafeMutablePointer<gss_qop_t>?) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_wrap(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ conf_req_flag: Int32, _ qop_req: gss_qop_t, _ input_message_buffer: gss_buffer_t, _ conf_state: UnsafeMutablePointer<Int32>?, _ output_message_buffer: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gss_wrap_size_limit(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ conf_req_flag: Int32, _ qop_req: gss_qop_t, _ req_output_size: OM_uint32, _ max_input_size: UnsafeMutablePointer<OM_uint32>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gsskrb5_extract_authz_data_from_sec_context(_ minor_status: UnsafeMutablePointer<OM_uint32>, _ context_handle: gss_ctx_id_t, _ ad_type: Int32, _ ad_data: gss_buffer_t) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func gsskrb5_register_acceptor_identity(_ identity: UnsafePointer<Int8>) -> OM_uint32
@available(OSX 10.7, *)
@discardableResult
func krb5_gss_register_acceptor_identity(_ identity: UnsafePointer<Int8>) -> OM_uint32
