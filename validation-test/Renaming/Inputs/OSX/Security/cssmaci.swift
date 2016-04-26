
struct cssm_spi_ac_funcs {
  var AuthCompute: (@convention(c) (CSSM_AC_HANDLE, UnsafePointer<cssm_tuplegroup>!, UnsafePointer<cssm_tuplegroup>!, uint32, UnsafePointer<cssm_list>!, UnsafePointer<cssm_list>!, UnsafePointer<cssm_list>!, UnsafeMutablePointer<cssm_tuplegroup>!) -> CSSM_RETURN)!
  var PassThrough: (@convention(c) (CSSM_AC_HANDLE, CSSM_TP_HANDLE, CSSM_CL_HANDLE, CSSM_CC_HANDLE, UnsafePointer<cssm_dl_db_list>!, uint32, UnsafePointer<Void>!, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> CSSM_RETURN)!
  init()
  init(AuthCompute AuthCompute: (@convention(c) (CSSM_AC_HANDLE, UnsafePointer<cssm_tuplegroup>!, UnsafePointer<cssm_tuplegroup>!, uint32, UnsafePointer<cssm_list>!, UnsafePointer<cssm_list>!, UnsafePointer<cssm_list>!, UnsafeMutablePointer<cssm_tuplegroup>!) -> CSSM_RETURN)!, PassThrough PassThrough: (@convention(c) (CSSM_AC_HANDLE, CSSM_TP_HANDLE, CSSM_CL_HANDLE, CSSM_CC_HANDLE, UnsafePointer<cssm_dl_db_list>!, uint32, UnsafePointer<Void>!, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!) -> CSSM_RETURN)!)
}
