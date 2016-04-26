
var CSSM_HINT_CALLBACK: Int32 { get }
typealias CSSM_MANAGER_EVENT_TYPES = uint32
var CSSM_MANAGER_SERVICE_REQUEST: Int32 { get }
var CSSM_MANAGER_REPLY: Int32 { get }
struct cssm_manager_event_notification {
  var DestinationModuleManagerType: CSSM_SERVICE_MASK
  var SourceModuleManagerType: CSSM_SERVICE_MASK
  var Event: CSSM_MANAGER_EVENT_TYPES
  var EventId: uint32
  var EventData: cssm_data
  init()
  init(DestinationModuleManagerType DestinationModuleManagerType: CSSM_SERVICE_MASK, SourceModuleManagerType SourceModuleManagerType: CSSM_SERVICE_MASK, Event Event: CSSM_MANAGER_EVENT_TYPES, EventId EventId: uint32, EventData EventData: cssm_data)
}
