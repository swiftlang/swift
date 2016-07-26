
typealias CSSM_SPI_ModuleEventHandler = @convention(c) (UnsafePointer<cssm_guid>!, UnsafeMutablePointer<Void>!, uint32, CSSM_SERVICE_TYPE, CSSM_MODULE_EVENT) -> CSSM_RETURN
typealias CSSM_CONTEXT_EVENT = uint32
var CSSM_CONTEXT_EVENT_CREATE: Int { get }
var CSSM_CONTEXT_EVENT_DELETE: Int { get }
var CSSM_CONTEXT_EVENT_UPDATE: Int { get }
struct cssm_module_funcs {
  var ServiceType: CSSM_SERVICE_TYPE
  var NumberOfServiceFuncs: uint32
  var ServiceFuncs: UnsafePointer<CSSM_PROC_ADDR?>!
  init()
  init(ServiceType ServiceType: CSSM_SERVICE_TYPE, NumberOfServiceFuncs NumberOfServiceFuncs: uint32, ServiceFuncs ServiceFuncs: UnsafePointer<CSSM_PROC_ADDR?>!)
}
struct cssm_upcalls {
  var malloc_func: (@convention(c) (CSSM_HANDLE, Int) -> UnsafeMutablePointer<Void>!)!
  var free_func: (@convention(c) (CSSM_HANDLE, UnsafeMutablePointer<Void>!) -> Void)!
  var realloc_func: (@convention(c) (CSSM_HANDLE, UnsafeMutablePointer<Void>!, Int) -> UnsafeMutablePointer<Void>!)!
  var calloc_func: (@convention(c) (CSSM_HANDLE, Int, Int) -> UnsafeMutablePointer<Void>!)!
  var CcToHandle_func: (@convention(c) (CSSM_CC_HANDLE, CSSM_MODULE_HANDLE_PTR!) -> CSSM_RETURN)!
  var GetModuleInfo_func: (@convention(c) (CSSM_MODULE_HANDLE, UnsafeMutablePointer<cssm_guid>!, UnsafeMutablePointer<cssm_version>!, UnsafeMutablePointer<uint32>!, UnsafeMutablePointer<CSSM_SERVICE_TYPE>!, UnsafeMutablePointer<CSSM_ATTACH_FLAGS>!, UnsafeMutablePointer<CSSM_KEY_HIERARCHY>!, CSSM_API_MEMORY_FUNCS_PTR!, UnsafeMutablePointer<cssm_func_name_addr>!, uint32) -> CSSM_RETURN)!
  init()
  init(malloc_func malloc_func: (@convention(c) (CSSM_HANDLE, Int) -> UnsafeMutablePointer<Void>!)!, free_func free_func: (@convention(c) (CSSM_HANDLE, UnsafeMutablePointer<Void>!) -> Void)!, realloc_func realloc_func: (@convention(c) (CSSM_HANDLE, UnsafeMutablePointer<Void>!, Int) -> UnsafeMutablePointer<Void>!)!, calloc_func calloc_func: (@convention(c) (CSSM_HANDLE, Int, Int) -> UnsafeMutablePointer<Void>!)!, CcToHandle_func CcToHandle_func: (@convention(c) (CSSM_CC_HANDLE, CSSM_MODULE_HANDLE_PTR!) -> CSSM_RETURN)!, GetModuleInfo_func GetModuleInfo_func: (@convention(c) (CSSM_MODULE_HANDLE, UnsafeMutablePointer<cssm_guid>!, UnsafeMutablePointer<cssm_version>!, UnsafeMutablePointer<uint32>!, UnsafeMutablePointer<CSSM_SERVICE_TYPE>!, UnsafeMutablePointer<CSSM_ATTACH_FLAGS>!, UnsafeMutablePointer<CSSM_KEY_HIERARCHY>!, CSSM_API_MEMORY_FUNCS_PTR!, UnsafeMutablePointer<cssm_func_name_addr>!, uint32) -> CSSM_RETURN)!)
}
