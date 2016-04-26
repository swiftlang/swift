
struct cssm_state_funcs {
  var cssm_GetAttachFunctions: (@convention(c) (CSSM_MODULE_HANDLE, CSSM_SERVICE_MASK, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!, UnsafeMutablePointer<cssm_guid>!, UnsafeMutablePointer<CSSM_BOOL>!) -> CSSM_RETURN)!
  var cssm_ReleaseAttachFunctions: (@convention(c) (CSSM_MODULE_HANDLE) -> CSSM_RETURN)!
  var cssm_GetAppMemoryFunctions: (@convention(c) (CSSM_MODULE_HANDLE, UnsafeMutablePointer<cssm_upcalls>!) -> CSSM_RETURN)!
  var cssm_IsFuncCallValid: (@convention(c) (CSSM_MODULE_HANDLE, CSSM_PROC_ADDR!, CSSM_PROC_ADDR!, CSSM_PRIVILEGE, UnsafeMutablePointer<CSSM_PRIVILEGE>!, CSSM_BITMASK, UnsafeMutablePointer<CSSM_BOOL>!) -> CSSM_RETURN)!
  var cssm_DeregisterManagerServices: (@convention(c) (UnsafePointer<cssm_guid>!) -> CSSM_RETURN)!
  var cssm_DeliverModuleManagerEvent: (@convention(c) (UnsafePointer<cssm_manager_event_notification>!) -> CSSM_RETURN)!
  init()
  init(cssm_GetAttachFunctions cssm_GetAttachFunctions: (@convention(c) (CSSM_MODULE_HANDLE, CSSM_SERVICE_MASK, UnsafeMutablePointer<UnsafeMutablePointer<Void>?>!, UnsafeMutablePointer<cssm_guid>!, UnsafeMutablePointer<CSSM_BOOL>!) -> CSSM_RETURN)!, cssm_ReleaseAttachFunctions cssm_ReleaseAttachFunctions: (@convention(c) (CSSM_MODULE_HANDLE) -> CSSM_RETURN)!, cssm_GetAppMemoryFunctions cssm_GetAppMemoryFunctions: (@convention(c) (CSSM_MODULE_HANDLE, UnsafeMutablePointer<cssm_upcalls>!) -> CSSM_RETURN)!, cssm_IsFuncCallValid cssm_IsFuncCallValid: (@convention(c) (CSSM_MODULE_HANDLE, CSSM_PROC_ADDR!, CSSM_PROC_ADDR!, CSSM_PRIVILEGE, UnsafeMutablePointer<CSSM_PRIVILEGE>!, CSSM_BITMASK, UnsafeMutablePointer<CSSM_BOOL>!) -> CSSM_RETURN)!, cssm_DeregisterManagerServices cssm_DeregisterManagerServices: (@convention(c) (UnsafePointer<cssm_guid>!) -> CSSM_RETURN)!, cssm_DeliverModuleManagerEvent cssm_DeliverModuleManagerEvent: (@convention(c) (UnsafePointer<cssm_manager_event_notification>!) -> CSSM_RETURN)!)
}
struct cssm_manager_registration_info {
  var Initialize: (@convention(c) (uint32, uint32) -> CSSM_RETURN)!
  var Terminate: (@convention(c) () -> CSSM_RETURN)!
  var RegisterDispatchTable: (@convention(c) (UnsafeMutablePointer<cssm_state_funcs>!) -> CSSM_RETURN)!
  var DeregisterDispatchTable: (@convention(c) () -> CSSM_RETURN)!
  var EventNotifyManager: (@convention(c) (UnsafePointer<cssm_manager_event_notification>!) -> CSSM_RETURN)!
  var RefreshFunctionTable: (@convention(c) (UnsafeMutablePointer<cssm_func_name_addr>!, uint32) -> CSSM_RETURN)!
  init()
  init(Initialize Initialize: (@convention(c) (uint32, uint32) -> CSSM_RETURN)!, Terminate Terminate: (@convention(c) () -> CSSM_RETURN)!, RegisterDispatchTable RegisterDispatchTable: (@convention(c) (UnsafeMutablePointer<cssm_state_funcs>!) -> CSSM_RETURN)!, DeregisterDispatchTable DeregisterDispatchTable: (@convention(c) () -> CSSM_RETURN)!, EventNotifyManager EventNotifyManager: (@convention(c) (UnsafePointer<cssm_manager_event_notification>!) -> CSSM_RETURN)!, RefreshFunctionTable RefreshFunctionTable: (@convention(c) (UnsafeMutablePointer<cssm_func_name_addr>!, uint32) -> CSSM_RETURN)!)
}
var CSSM_HINT_NONE: Int { get }
var CSSM_HINT_ADDRESS_APP: Int { get }
var CSSM_HINT_ADDRESS_SP: Int { get }
