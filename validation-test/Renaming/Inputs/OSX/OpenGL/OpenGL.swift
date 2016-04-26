
var CGL_VERSION_1_0: Int32 { get }
var CGL_VERSION_1_1: Int32 { get }
var CGL_VERSION_1_2: Int32 { get }
var CGL_VERSION_1_3: Int32 { get }
@discardableResult
func CGLChoosePixelFormat(_ attribs: UnsafePointer<CGLPixelFormatAttribute>, _ pix: UnsafeMutablePointer<CGLPixelFormatObj?>, _ npix: UnsafeMutablePointer<GLint>) -> CGLError
@discardableResult
func CGLDestroyPixelFormat(_ pix: CGLPixelFormatObj) -> CGLError
@discardableResult
func CGLDescribePixelFormat(_ pix: CGLPixelFormatObj, _ pix_num: GLint, _ attrib: CGLPixelFormatAttribute, _ value: UnsafeMutablePointer<GLint>) -> CGLError
@available(OSX 10.5, *)
func CGLReleasePixelFormat(_ pix: CGLPixelFormatObj)
@available(OSX 10.5, *)
@discardableResult
func CGLRetainPixelFormat(_ pix: CGLPixelFormatObj) -> CGLPixelFormatObj
@available(OSX 10.5, *)
@discardableResult
func CGLGetPixelFormatRetainCount(_ pix: CGLPixelFormatObj) -> GLuint
@discardableResult
func CGLQueryRendererInfo(_ display_mask: GLuint, _ rend: UnsafeMutablePointer<CGLRendererInfoObj?>, _ nrend: UnsafeMutablePointer<GLint>) -> CGLError
@discardableResult
func CGLDestroyRendererInfo(_ rend: CGLRendererInfoObj) -> CGLError
@discardableResult
func CGLDescribeRenderer(_ rend: CGLRendererInfoObj, _ rend_num: GLint, _ prop: CGLRendererProperty, _ value: UnsafeMutablePointer<GLint>?) -> CGLError
@discardableResult
func CGLCreateContext(_ pix: CGLPixelFormatObj, _ share: CGLContextObj?, _ ctx: UnsafeMutablePointer<CGLContextObj?>) -> CGLError
@discardableResult
func CGLDestroyContext(_ ctx: CGLContextObj) -> CGLError
@available(OSX 10.5, *)
@discardableResult
func CGLRetainContext(_ ctx: CGLContextObj) -> CGLContextObj
@available(OSX 10.5, *)
func CGLReleaseContext(_ ctx: CGLContextObj)
@available(OSX 10.5, *)
@discardableResult
func CGLGetContextRetainCount(_ ctx: CGLContextObj) -> GLuint
@available(OSX 10.5, *)
@discardableResult
func CGLGetPixelFormat(_ ctx: CGLContextObj) -> CGLPixelFormatObj?
@discardableResult
func CGLClearDrawable(_ ctx: CGLContextObj) -> CGLError
@discardableResult
func CGLFlushDrawable(_ ctx: CGLContextObj) -> CGLError
@discardableResult
func CGLEnable(_ ctx: CGLContextObj, _ pname: CGLContextEnable) -> CGLError
@discardableResult
func CGLDisable(_ ctx: CGLContextObj, _ pname: CGLContextEnable) -> CGLError
@discardableResult
func CGLIsEnabled(_ ctx: CGLContextObj, _ pname: CGLContextEnable, _ enable: UnsafeMutablePointer<GLint>) -> CGLError
@discardableResult
func CGLSetParameter(_ ctx: CGLContextObj, _ pname: CGLContextParameter, _ params: UnsafePointer<GLint>) -> CGLError
@discardableResult
func CGLGetParameter(_ ctx: CGLContextObj, _ pname: CGLContextParameter, _ params: UnsafeMutablePointer<GLint>) -> CGLError
@discardableResult
func CGLSetVirtualScreen(_ ctx: CGLContextObj, _ screen: GLint) -> CGLError
@discardableResult
func CGLGetVirtualScreen(_ ctx: CGLContextObj, _ screen: UnsafeMutablePointer<GLint>) -> CGLError
@available(OSX 10.7, *)
@discardableResult
func CGLUpdateContext(_ ctx: CGLContextObj) -> CGLError
@available(OSX 10.6, *)
@discardableResult
func CGLSetGlobalOption(_ pname: CGLGlobalOption, _ params: UnsafePointer<GLint>?) -> CGLError
@available(OSX 10.6, *)
@discardableResult
func CGLGetGlobalOption(_ pname: CGLGlobalOption, _ params: UnsafeMutablePointer<GLint>) -> CGLError
@discardableResult
func CGLSetOption(_ pname: CGLGlobalOption, _ param: GLint) -> CGLError
@discardableResult
func CGLGetOption(_ pname: CGLGlobalOption, _ param: UnsafeMutablePointer<GLint>) -> CGLError
@available(OSX 10.4, *)
@discardableResult
func CGLLockContext(_ ctx: CGLContextObj) -> CGLError
@available(OSX 10.4, *)
@discardableResult
func CGLUnlockContext(_ ctx: CGLContextObj) -> CGLError
func CGLGetVersion(_ majorvers: UnsafeMutablePointer<GLint>?, _ minorvers: UnsafeMutablePointer<GLint>?)
@discardableResult
func CGLErrorString(_ error: CGLError) -> UnsafePointer<Int8>
