
enum operating_modes_t : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case VMNET_HOST_MODE
  case VMNET_SHARED_MODE
}
struct interface_event_t : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var VMNET_INTERFACE_PACKETS_AVAILABLE: interface_event_t { get }
}
enum vmnet_return_t : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case VMNET_SUCCESS
  case VMNET_FAILURE
  case VMNET_MEM_FAILURE
  case VMNET_INVALID_ARGUMENT
  case VMNET_SETUP_INCOMPLETE
  case VMNET_INVALID_ACCESS
  case VMNET_PACKET_TOO_BIG
  case VMNET_BUFFER_EXHAUSTED
  case VMNET_TOO_MANY_PACKETS
}
struct vmpktdesc {
  var vm_pkt_size: Int
  var vm_pkt_iov: UnsafeMutablePointer<iovec>
  var vm_pkt_iovcnt: UInt32
  var vm_flags: UInt32
}
typealias interface_ref = OpaquePointer
var vmnet_operation_mode_key: UnsafePointer<Int8>
var vmnet_interface_id_key: UnsafePointer<Int8>
var vmnet_mac_address_key: UnsafePointer<Int8>
var vmnet_mtu_key: UnsafePointer<Int8>
var vmnet_max_packet_size_key: UnsafePointer<Int8>
var vmnet_estimated_packets_available_key: UnsafePointer<Int8>
@discardableResult
func vmnet_start_interface(_ interface_desc: xpc_object_t, _ queue: dispatch_queue_t, _ handler: (vmnet_return_t, xpc_object_t?) -> Void) -> interface_ref?
@discardableResult
func vmnet_interface_set_event_callback(_ interface: interface_ref, _ flags: interface_event_t, _ queue: dispatch_queue_t?, _ handler: ((interface_event_t, xpc_object_t) -> Void)?) -> vmnet_return_t
@discardableResult
func vmnet_write(_ interface: interface_ref, _ packets: UnsafeMutablePointer<vmpktdesc>, _ pktcnt: UnsafeMutablePointer<Int32>) -> vmnet_return_t
@discardableResult
func vmnet_read(_ interface: interface_ref, _ packets: UnsafeMutablePointer<vmpktdesc>, _ pktcnt: UnsafeMutablePointer<Int32>) -> vmnet_return_t
@discardableResult
func vmnet_stop_interface(_ interface: interface_ref, _ queue: dispatch_queue_t, _ handler: (vmnet_return_t) -> Void) -> vmnet_return_t
