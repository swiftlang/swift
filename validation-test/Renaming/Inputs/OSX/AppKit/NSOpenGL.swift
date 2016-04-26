
var NSOPENGL_CURRENT_VERSION: Int32 { get }
struct NSOpenGLGlobalOption : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var NSOpenGLGOFormatCacheSize: NSOpenGLGlobalOption { get }
var NSOpenGLGOClearFormatCache: NSOpenGLGlobalOption { get }
var NSOpenGLGORetainRenderers: NSOpenGLGlobalOption { get }
var NSOpenGLGOUseBuildCache: NSOpenGLGlobalOption { get }
func NSOpenGLSetOption(_ pname: NSOpenGLGlobalOption, _ param: GLint)
func NSOpenGLGetOption(_ pname: NSOpenGLGlobalOption, _ param: UnsafeMutablePointer<GLint>)
func NSOpenGLGetVersion(_ major: UnsafeMutablePointer<GLint>?, _ minor: UnsafeMutablePointer<GLint>?)
var NSOpenGLPFAAllRenderers: Int { get }
var NSOpenGLPFATripleBuffer: Int { get }
var NSOpenGLPFADoubleBuffer: Int { get }
var NSOpenGLPFAStereo: Int { get }
var NSOpenGLPFAAuxBuffers: Int { get }
var NSOpenGLPFAColorSize: Int { get }
var NSOpenGLPFAAlphaSize: Int { get }
var NSOpenGLPFADepthSize: Int { get }
var NSOpenGLPFAStencilSize: Int { get }
var NSOpenGLPFAAccumSize: Int { get }
var NSOpenGLPFAMinimumPolicy: Int { get }
var NSOpenGLPFAMaximumPolicy: Int { get }
var NSOpenGLPFASampleBuffers: Int { get }
var NSOpenGLPFASamples: Int { get }
var NSOpenGLPFAAuxDepthStencil: Int { get }
var NSOpenGLPFAColorFloat: Int { get }
var NSOpenGLPFAMultisample: Int { get }
var NSOpenGLPFASupersample: Int { get }
var NSOpenGLPFASampleAlpha: Int { get }
var NSOpenGLPFARendererID: Int { get }
var NSOpenGLPFANoRecovery: Int { get }
var NSOpenGLPFAAccelerated: Int { get }
var NSOpenGLPFAClosestPolicy: Int { get }
var NSOpenGLPFABackingStore: Int { get }
var NSOpenGLPFAScreenMask: Int { get }
@available(OSX 10.5, *)
var NSOpenGLPFAAllowOfflineRenderers: Int { get }
var NSOpenGLPFAAcceleratedCompute: Int { get }
var NSOpenGLPFAVirtualScreenCount: Int { get }
@available(OSX 10.7, *)
var NSOpenGLPFAOpenGLProfile: Int { get }
typealias NSOpenGLPixelFormatAttribute = UInt32
@available(OSX 10.7, *)
var NSOpenGLProfileVersionLegacy: Int { get }
@available(OSX 10.7, *)
var NSOpenGLProfileVersion3_2Core: Int { get }
@available(OSX 10.10, *)
var NSOpenGLProfileVersion4_1Core: Int { get }
class NSOpenGLPixelFormat : NSObject, NSCoding {
  init?(attributes attribs: UnsafePointer<NSOpenGLPixelFormatAttribute>)
  @available(OSX 10.6, *)
  init?(cglPixelFormatObj format: OpaquePointer)
  func getValues(_ vals: UnsafeMutablePointer<GLint>, forAttribute attrib: NSOpenGLPixelFormatAttribute, forVirtualScreen screen: GLint)
  var numberOfVirtualScreens: GLint { get }
  var cglPixelFormatObj: OpaquePointer? { get }
}
enum NSOpenGLContextParameter : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case glcpSwapInterval
  case glcpSurfaceOrder
  case glcpSurfaceOpacity
  case glcpSurfaceBackingSize
  case glcpReclaimResources
  case glcpCurrentRendererID
  case glcpgpuVertexProcessing
  case glcpgpuFragmentProcessing
  case glcpHasDrawable
  case glcpmpSwapsInFlight
  case glcpSwapRectangle
  case glcpSwapRectangleEnable
  case glcpRasterizationEnable
  case glcpStateValidation
  case glcpSurfaceSurfaceVolatile
}
typealias NSOpenGLContextAuxiliary = _CGLContextObject
class NSOpenGLContext : NSObject, NSLocking {
  init?(format format: NSOpenGLPixelFormat, share share: NSOpenGLContext?)
  @available(OSX 10.6, *)
  init?(cglContextObj context: UnsafeMutablePointer<_CGLContextObject>)
  @available(OSX 10.10, *)
  var pixelFormat: NSOpenGLPixelFormat { get }
  unowned(unsafe) var view: @sil_unmanaged NSView?
  func clearDrawable()
  func update()
  func flushBuffer()
  func makeCurrentContext()
  class func clearCurrentContext()
  @discardableResult
  class func current() -> NSOpenGLContext?
  func setValues(_ vals: UnsafePointer<GLint>, for param: NSOpenGLContextParameter)
  func getValues(_ vals: UnsafeMutablePointer<GLint>, for param: NSOpenGLContextParameter)
  var currentVirtualScreen: GLint
  var cglContextObj: UnsafeMutablePointer<_CGLContextObject>? { get }
}
extension NSOpenGLContext {
}
