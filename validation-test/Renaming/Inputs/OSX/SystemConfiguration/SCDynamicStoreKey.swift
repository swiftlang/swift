
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateNetworkGlobalEntity(_ allocator: CFAllocator?, _ domain: CFString, _ entity: CFString) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateNetworkInterface(_ allocator: CFAllocator?, _ domain: CFString) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateNetworkInterfaceEntity(_ allocator: CFAllocator?, _ domain: CFString, _ ifname: CFString, _ entity: CFString?) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateNetworkServiceEntity(_ allocator: CFAllocator?, _ domain: CFString, _ serviceID: CFString, _ entity: CFString?) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateComputerName(_ allocator: CFAllocator?) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateConsoleUser(_ allocator: CFAllocator?) -> CFString
@available(OSX 10.2, *)
@discardableResult
func SCDynamicStoreKeyCreateHostNames(_ allocator: CFAllocator?) -> CFString
@available(OSX 10.2, *)
@discardableResult
func SCDynamicStoreKeyCreateLocation(_ allocator: CFAllocator?) -> CFString
@available(OSX 10.1, *)
@discardableResult
func SCDynamicStoreKeyCreateProxies(_ allocator: CFAllocator?) -> CFString
