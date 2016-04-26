
var HV_VM_DEFAULT: Int { get }
typealias hv_vm_options_t = UInt64
var HV_VCPU_DEFAULT: Int { get }
typealias hv_vcpu_options_t = UInt64
var HV_MEMORY_READ: Int { get }
var HV_MEMORY_WRITE: Int { get }
var HV_MEMORY_EXEC: Int { get }
typealias hv_memory_flags_t = UInt64
typealias hv_vcpuid_t = UInt32
typealias hv_uvaddr_t = UnsafePointer<Void>
typealias hv_gpaddr_t = UInt64
