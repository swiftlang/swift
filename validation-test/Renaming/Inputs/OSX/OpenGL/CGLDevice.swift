
@available(OSX 10.6, *)
typealias CGLShareGroupObj = OpaquePointer
@available(OSX 10.6, *)
@discardableResult
func CGLGetShareGroup(_ ctx: CGLContextObj) -> CGLShareGroupObj?
@available(OSX 10.10, *)
typealias cl_device_id = OpaquePointer
@available(OSX 10.10, *)
@discardableResult
func CGLGetDeviceFromGLRenderer(_ rendererID: GLint) -> cl_device_id
