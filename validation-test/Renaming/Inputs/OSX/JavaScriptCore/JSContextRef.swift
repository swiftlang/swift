
@available(OSX 10.6, *)
@discardableResult
func JSContextGroupCreate() -> JSContextGroupRef!
@available(OSX 10.6, *)
@discardableResult
func JSContextGroupRetain(_ group: JSContextGroupRef!) -> JSContextGroupRef!
@available(OSX 10.6, *)
func JSContextGroupRelease(_ group: JSContextGroupRef!)
@available(OSX 10.5, *)
@discardableResult
func JSGlobalContextCreate(_ globalObjectClass: JSClassRef!) -> JSGlobalContextRef!
@available(OSX 10.6, *)
@discardableResult
func JSGlobalContextCreateInGroup(_ group: JSContextGroupRef!, _ globalObjectClass: JSClassRef!) -> JSGlobalContextRef!
@discardableResult
func JSGlobalContextRetain(_ ctx: JSGlobalContextRef!) -> JSGlobalContextRef!
func JSGlobalContextRelease(_ ctx: JSGlobalContextRef!)
@discardableResult
func JSContextGetGlobalObject(_ ctx: JSContextRef!) -> JSObjectRef!
@available(OSX 10.6, *)
@discardableResult
func JSContextGetGroup(_ ctx: JSContextRef!) -> JSContextGroupRef!
@available(OSX 10.7, *)
@discardableResult
func JSContextGetGlobalContext(_ ctx: JSContextRef!) -> JSGlobalContextRef!
@available(OSX 10.10, *)
@discardableResult
func JSGlobalContextCopyName(_ ctx: JSGlobalContextRef!) -> JSStringRef!
@available(OSX 10.10, *)
func JSGlobalContextSetName(_ ctx: JSGlobalContextRef!, _ name: JSStringRef!)
