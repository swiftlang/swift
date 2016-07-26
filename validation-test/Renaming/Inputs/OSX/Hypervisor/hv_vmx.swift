
@available(OSX 10.10, *)
@discardableResult
func hv_vmx_vcpu_read_vmcs(_ vcpu: hv_vcpuid_t, _ field: UInt32, _ value: UnsafeMutablePointer<UInt64>!) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vmx_vcpu_write_vmcs(_ vcpu: hv_vcpuid_t, _ field: UInt32, _ value: UInt64) -> hv_return_t
struct hv_vmx_capability_t : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var HV_VMX_CAP_PINBASED: hv_vmx_capability_t { get }
var HV_VMX_CAP_PROCBASED: hv_vmx_capability_t { get }
var HV_VMX_CAP_PROCBASED2: hv_vmx_capability_t { get }
var HV_VMX_CAP_ENTRY: hv_vmx_capability_t { get }
var HV_VMX_CAP_EXIT: hv_vmx_capability_t { get }
var HV_VMX_CAP_PREEMPTION_TIMER: hv_vmx_capability_t { get }
@available(OSX 10.10, *)
@discardableResult
func hv_vmx_read_capability(_ field: hv_vmx_capability_t, _ value: UnsafeMutablePointer<UInt64>!) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vmx_vcpu_set_apic_address(_ vcpu: hv_vcpuid_t, _ gpa: hv_gpaddr_t) -> hv_return_t
