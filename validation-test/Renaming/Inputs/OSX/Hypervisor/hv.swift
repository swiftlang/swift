
@available(OSX 10.10, *)
@discardableResult
func hv_vm_create(_ flags: hv_vm_options_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vm_destroy() -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vm_map(_ uva: hv_uvaddr_t!, _ gpa: hv_gpaddr_t, _ size: Int, _ flags: hv_memory_flags_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vm_unmap(_ gpa: hv_gpaddr_t, _ size: Int) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vm_protect(_ gpa: hv_gpaddr_t, _ size: Int, _ flags: hv_memory_flags_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vm_sync_tsc(_ tsc: UInt64) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_create(_ vcpu: UnsafeMutablePointer<hv_vcpuid_t>!, _ flags: hv_vcpu_options_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_destroy(_ vcpu: hv_vcpuid_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_read_register(_ vcpu: hv_vcpuid_t, _ reg: hv_x86_reg_t, _ value: UnsafeMutablePointer<UInt64>!) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_write_register(_ vcpu: hv_vcpuid_t, _ reg: hv_x86_reg_t, _ value: UInt64) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_read_fpstate(_ vcpu: hv_vcpuid_t, _ buffer: UnsafeMutablePointer<Void>!, _ size: Int) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_write_fpstate(_ vcpu: hv_vcpuid_t, _ buffer: UnsafeMutablePointer<Void>!, _ size: Int) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_enable_native_msr(_ vcpu: hv_vcpuid_t, _ msr: UInt32, _ enable: Bool) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_read_msr(_ vcpu: hv_vcpuid_t, _ msr: UInt32, _ value: UnsafeMutablePointer<UInt64>!) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_write_msr(_ vcpu: hv_vcpuid_t, _ msr: UInt32, _ value: UInt64) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_flush(_ vcpu: hv_vcpuid_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_invalidate_tlb(_ vcpu: hv_vcpuid_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_run(_ vcpu: hv_vcpuid_t) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_interrupt(_ vcpus: UnsafeMutablePointer<hv_vcpuid_t>!, _ vcpu_count: UInt32) -> hv_return_t
@available(OSX 10.10, *)
@discardableResult
func hv_vcpu_get_exec_time(_ vcpu: hv_vcpuid_t, _ time: UnsafeMutablePointer<UInt64>!) -> hv_return_t
