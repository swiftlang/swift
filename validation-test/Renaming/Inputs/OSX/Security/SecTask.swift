
class SecTask {
}
@discardableResult
func SecTaskGetTypeID() -> CFTypeID
@discardableResult
func SecTaskCreateWithAuditToken(_ allocator: CFAllocator?, _ token: audit_token_t) -> SecTask?
@discardableResult
func SecTaskCreateFromSelf(_ allocator: CFAllocator?) -> SecTask?
@discardableResult
func SecTaskCopyValueForEntitlement(_ task: SecTask, _ entitlement: CFString, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFTypeRef?
@discardableResult
func SecTaskCopyValuesForEntitlements(_ task: SecTask, _ entitlements: CFArray, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFDictionary?
