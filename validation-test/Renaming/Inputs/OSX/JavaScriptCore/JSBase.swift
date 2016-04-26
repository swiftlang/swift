
typealias JSContextGroupRef = OpaquePointer
typealias JSContextRef = OpaquePointer
typealias JSGlobalContextRef = OpaquePointer
typealias JSStringRef = OpaquePointer
typealias JSClassRef = OpaquePointer
typealias JSPropertyNameArrayRef = OpaquePointer
typealias JSPropertyNameAccumulatorRef = OpaquePointer
typealias JSValueRef = OpaquePointer
typealias JSObjectRef = OpaquePointer
@discardableResult
func JSEvaluateScript(_ ctx: JSContextRef!, _ script: JSStringRef!, _ thisObject: JSObjectRef!, _ sourceURL: JSStringRef!, _ startingLineNumber: Int32, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> JSValueRef!
@discardableResult
func JSCheckScriptSyntax(_ ctx: JSContextRef!, _ script: JSStringRef!, _ sourceURL: JSStringRef!, _ startingLineNumber: Int32, _ exception: UnsafeMutablePointer<JSValueRef?>!) -> Bool
func JSGarbageCollect(_ ctx: JSContextRef!)
