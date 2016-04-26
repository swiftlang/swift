
typealias CGLContextObj = UnsafeMutablePointer<_CGLContextObject>
typealias CGLPixelFormatObj = OpaquePointer
typealias CGLRendererInfoObj = OpaquePointer
struct _CGLPixelFormatAttribute : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLPFAAllRenderers: _CGLPixelFormatAttribute { get }
@available(OSX 10.7, *)
var kCGLPFATripleBuffer: _CGLPixelFormatAttribute { get }
var kCGLPFADoubleBuffer: _CGLPixelFormatAttribute { get }
var kCGLPFAColorSize: _CGLPixelFormatAttribute { get }
var kCGLPFAAlphaSize: _CGLPixelFormatAttribute { get }
var kCGLPFADepthSize: _CGLPixelFormatAttribute { get }
var kCGLPFAStencilSize: _CGLPixelFormatAttribute { get }
var kCGLPFAMinimumPolicy: _CGLPixelFormatAttribute { get }
var kCGLPFAMaximumPolicy: _CGLPixelFormatAttribute { get }
@available(OSX 10.2, *)
var kCGLPFASampleBuffers: _CGLPixelFormatAttribute { get }
@available(OSX 10.2, *)
var kCGLPFASamples: _CGLPixelFormatAttribute { get }
@available(OSX 10.2, *)
var kCGLPFAColorFloat: _CGLPixelFormatAttribute { get }
@available(OSX 10.3, *)
var kCGLPFAMultisample: _CGLPixelFormatAttribute { get }
@available(OSX 10.3, *)
var kCGLPFASupersample: _CGLPixelFormatAttribute { get }
@available(OSX 10.3, *)
var kCGLPFASampleAlpha: _CGLPixelFormatAttribute { get }
var kCGLPFARendererID: _CGLPixelFormatAttribute { get }
var kCGLPFANoRecovery: _CGLPixelFormatAttribute { get }
var kCGLPFAAccelerated: _CGLPixelFormatAttribute { get }
var kCGLPFAClosestPolicy: _CGLPixelFormatAttribute { get }
var kCGLPFABackingStore: _CGLPixelFormatAttribute { get }
@available(OSX 10.7, *)
var kCGLPFABackingVolatile: _CGLPixelFormatAttribute { get }
var kCGLPFADisplayMask: _CGLPixelFormatAttribute { get }
@available(OSX 10.5, *)
var kCGLPFAAllowOfflineRenderers: _CGLPixelFormatAttribute { get }
@available(OSX 10.6, *)
var kCGLPFAAcceleratedCompute: _CGLPixelFormatAttribute { get }
@available(OSX 10.7, *)
var kCGLPFAOpenGLProfile: _CGLPixelFormatAttribute { get }
@available(OSX 10.8, *)
var kCGLPFASupportsAutomaticGraphicsSwitching: _CGLPixelFormatAttribute { get }
var kCGLPFAVirtualScreenCount: _CGLPixelFormatAttribute { get }
var kCGLPFAAuxBuffers: _CGLPixelFormatAttribute { get }
var kCGLPFAAccumSize: _CGLPixelFormatAttribute { get }
@available(OSX 10.2, *)
var kCGLPFAAuxDepthStencil: _CGLPixelFormatAttribute { get }
@available(OSX, introduced: 10.0, deprecated: 10.11)
var kCGLPFAStereo: _CGLPixelFormatAttribute { get }
typealias CGLPixelFormatAttribute = _CGLPixelFormatAttribute
struct _CGLRendererProperty : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLRPOffScreen: _CGLRendererProperty { get }
var kCGLRPRendererID: _CGLRendererProperty { get }
var kCGLRPAccelerated: _CGLRendererProperty { get }
var kCGLRPBackingStore: _CGLRendererProperty { get }
var kCGLRPWindow: _CGLRendererProperty { get }
var kCGLRPCompliant: _CGLRendererProperty { get }
var kCGLRPDisplayMask: _CGLRendererProperty { get }
var kCGLRPBufferModes: _CGLRendererProperty { get }
var kCGLRPColorModes: _CGLRendererProperty { get }
var kCGLRPAccumModes: _CGLRendererProperty { get }
var kCGLRPDepthModes: _CGLRendererProperty { get }
var kCGLRPStencilModes: _CGLRendererProperty { get }
var kCGLRPMaxAuxBuffers: _CGLRendererProperty { get }
@available(OSX 10.2, *)
var kCGLRPMaxSampleBuffers: _CGLRendererProperty { get }
@available(OSX 10.2, *)
var kCGLRPMaxSamples: _CGLRendererProperty { get }
@available(OSX 10.3, *)
var kCGLRPSampleModes: _CGLRendererProperty { get }
@available(OSX 10.3, *)
var kCGLRPSampleAlpha: _CGLRendererProperty { get }
@available(OSX 10.4, *)
var kCGLRPGPUVertProcCapable: _CGLRendererProperty { get }
@available(OSX 10.4, *)
var kCGLRPGPUFragProcCapable: _CGLRendererProperty { get }
var kCGLRPRendererCount: _CGLRendererProperty { get }
@available(OSX 10.5, *)
var kCGLRPOnline: _CGLRendererProperty { get }
@available(OSX 10.6, *)
var kCGLRPAcceleratedCompute: _CGLRendererProperty { get }
@available(OSX 10.7, *)
var kCGLRPVideoMemoryMegabytes: _CGLRendererProperty { get }
@available(OSX 10.7, *)
var kCGLRPTextureMemoryMegabytes: _CGLRendererProperty { get }
@available(OSX 10.9, *)
var kCGLRPMajorGLVersion: _CGLRendererProperty { get }
typealias CGLRendererProperty = _CGLRendererProperty
struct _CGLContextEnable : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLCESwapRectangle: _CGLContextEnable { get }
@available(OSX 10.2, *)
var kCGLCESwapLimit: _CGLContextEnable { get }
var kCGLCERasterization: _CGLContextEnable { get }
var kCGLCEStateValidation: _CGLContextEnable { get }
@available(OSX 10.3, *)
var kCGLCESurfaceBackingSize: _CGLContextEnable { get }
@available(OSX 10.3, *)
var kCGLCEDisplayListOptimization: _CGLContextEnable { get }
@available(OSX 10.4, *)
var kCGLCEMPEngine: _CGLContextEnable { get }
@available(OSX 10.7, *)
var kCGLCECrashOnRemovedFunctions: _CGLContextEnable { get }
typealias CGLContextEnable = _CGLContextEnable
struct _CGLGPURestartStatus : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLCPGPURestartStatusNone: _CGLGPURestartStatus { get }
var kCGLCPGPURestartStatusCaused: _CGLGPURestartStatus { get }
var kCGLCPGPURestartStatusBlacklisted: _CGLGPURestartStatus { get }
typealias CGLGPURestartStatus = _CGLGPURestartStatus
struct _CGLContextParameter : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLCPSwapRectangle: _CGLContextParameter { get }
var kCGLCPSwapInterval: _CGLContextParameter { get }
@available(OSX 10.3, *)
var kCGLCPDispatchTableSize: _CGLContextParameter { get }
var kCGLCPClientStorage: _CGLContextParameter { get }
@available(OSX 10.2, *)
var kCGLCPSurfaceOrder: _CGLContextParameter { get }
@available(OSX 10.2, *)
var kCGLCPSurfaceOpacity: _CGLContextParameter { get }
@available(OSX 10.3, *)
var kCGLCPSurfaceBackingSize: _CGLContextParameter { get }
@available(OSX 10.3, *)
var kCGLCPSurfaceSurfaceVolatile: _CGLContextParameter { get }
@available(OSX 10.4, *)
var kCGLCPReclaimResources: _CGLContextParameter { get }
@available(OSX 10.4, *)
var kCGLCPCurrentRendererID: _CGLContextParameter { get }
@available(OSX 10.4, *)
var kCGLCPGPUVertexProcessing: _CGLContextParameter { get }
@available(OSX 10.4, *)
var kCGLCPGPUFragmentProcessing: _CGLContextParameter { get }
@available(OSX 10.5, *)
var kCGLCPHasDrawable: _CGLContextParameter { get }
@available(OSX 10.5, *)
var kCGLCPMPSwapsInFlight: _CGLContextParameter { get }
@available(OSX 10.10, *)
var kCGLCPGPURestartStatus: _CGLContextParameter { get }
@available(OSX 10.10, *)
var kCGLCPAbortOnGPURestartStatusBlacklisted: _CGLContextParameter { get }
@available(OSX 10.10, *)
var kCGLCPSupportGPURestart: _CGLContextParameter { get }
@available(OSX 10.10, *)
var kCGLCPSupportSeparateAddressSpace: _CGLContextParameter { get }
@available(OSX 10.10, *)
var kCGLCPContextPriorityRequest: _CGLContextParameter { get }
typealias CGLContextParameter = _CGLContextParameter
struct CGLCPContextPriorityRequest : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLCPContextPriorityRequestHigh: CGLCPContextPriorityRequest { get }
var kCGLCPContextPriorityRequestNormal: CGLCPContextPriorityRequest { get }
var kCGLCPContextPriorityRequestLow: CGLCPContextPriorityRequest { get }
struct _CGLGlobalOption : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLGOFormatCacheSize: _CGLGlobalOption { get }
var kCGLGOClearFormatCache: _CGLGlobalOption { get }
var kCGLGORetainRenderers: _CGLGlobalOption { get }
@available(OSX 10.5, *)
var kCGLGOUseBuildCache: _CGLGlobalOption { get }
typealias CGLGlobalOption = _CGLGlobalOption
struct _CGLOpenGLProfile : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
@available(OSX 10.7, *)
var kCGLOGLPVersion_Legacy: _CGLOpenGLProfile { get }
@available(OSX 10.7, *)
var kCGLOGLPVersion_3_2_Core: _CGLOpenGLProfile { get }
@available(OSX 10.7, *)
var kCGLOGLPVersion_GL3_Core: _CGLOpenGLProfile { get }
@available(OSX 10.9, *)
var kCGLOGLPVersion_GL4_Core: _CGLOpenGLProfile { get }
@available(OSX 10.7, *)
typealias CGLOpenGLProfile = _CGLOpenGLProfile
struct _CGLError : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var kCGLNoError: _CGLError { get }
var kCGLBadAttribute: _CGLError { get }
var kCGLBadProperty: _CGLError { get }
var kCGLBadPixelFormat: _CGLError { get }
var kCGLBadRendererInfo: _CGLError { get }
var kCGLBadContext: _CGLError { get }
var kCGLBadDrawable: _CGLError { get }
var kCGLBadDisplay: _CGLError { get }
var kCGLBadState: _CGLError { get }
var kCGLBadValue: _CGLError { get }
var kCGLBadMatch: _CGLError { get }
var kCGLBadEnumeration: _CGLError { get }
var kCGLBadOffScreen: _CGLError { get }
var kCGLBadFullScreen: _CGLError { get }
var kCGLBadWindow: _CGLError { get }
var kCGLBadAddress: _CGLError { get }
var kCGLBadCodeModule: _CGLError { get }
var kCGLBadAlloc: _CGLError { get }
var kCGLBadConnection: _CGLError { get }
typealias CGLError = _CGLError
var kCGLMonoscopicBit: Int32 { get }
var kCGLStereoscopicBit: Int32 { get }
var kCGLSingleBufferBit: Int32 { get }
var kCGLDoubleBufferBit: Int32 { get }
var kCGLTripleBufferBit: Int32 { get }
var kCGL0Bit: Int32 { get }
var kCGL1Bit: Int32 { get }
var kCGL2Bit: Int32 { get }
var kCGL3Bit: Int32 { get }
var kCGL4Bit: Int32 { get }
var kCGL5Bit: Int32 { get }
var kCGL6Bit: Int32 { get }
var kCGL8Bit: Int32 { get }
var kCGL10Bit: Int32 { get }
var kCGL12Bit: Int32 { get }
var kCGL16Bit: Int32 { get }
var kCGL24Bit: Int32 { get }
var kCGL32Bit: Int32 { get }
var kCGL48Bit: Int32 { get }
var kCGL64Bit: Int32 { get }
var kCGL96Bit: Int32 { get }
var kCGL128Bit: Int32 { get }
var kCGLRGB444Bit: Int32 { get }
var kCGLARGB4444Bit: Int32 { get }
var kCGLRGB444A8Bit: Int32 { get }
var kCGLRGB555Bit: Int32 { get }
var kCGLARGB1555Bit: Int32 { get }
var kCGLRGB555A8Bit: Int32 { get }
var kCGLRGB565Bit: Int32 { get }
var kCGLRGB565A8Bit: Int32 { get }
var kCGLRGB888Bit: Int32 { get }
var kCGLARGB8888Bit: Int32 { get }
var kCGLRGB888A8Bit: Int32 { get }
var kCGLRGB101010Bit: Int32 { get }
var kCGLARGB2101010Bit: Int32 { get }
var kCGLRGB101010_A8Bit: Int32 { get }
var kCGLRGB121212Bit: Int32 { get }
var kCGLARGB12121212Bit: Int32 { get }
var kCGLRGB161616Bit: Int32 { get }
var kCGLRGBA16161616Bit: Int32 { get }
var kCGLRGBFloat64Bit: Int32 { get }
var kCGLRGBAFloat64Bit: Int32 { get }
var kCGLRGBFloat128Bit: Int32 { get }
var kCGLRGBAFloat128Bit: Int32 { get }
var kCGLRGBFloat256Bit: Int32 { get }
var kCGLRGBAFloat256Bit: Int32 { get }
var kCGLSupersampleBit: Int32 { get }
var kCGLMultisampleBit: Int32 { get }
var kCGLARGB16161616Bit: Int32 { get }
