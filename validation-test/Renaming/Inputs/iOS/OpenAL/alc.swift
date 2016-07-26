
var ALC_INVALID: Int32 { get }
var ALC_VERSION_0_1: Int32 { get }
typealias ALCboolean = Int8
typealias ALCchar = Int8
typealias ALCbyte = Int8
typealias ALCubyte = UInt8
typealias ALCshort = Int16
typealias ALCushort = UInt16
typealias ALCint = Int32
typealias ALCuint = UInt32
typealias ALCsizei = Int32
typealias ALCenum = Int32
typealias ALCfloat = Float
typealias ALCdouble = Double
var ALC_FALSE: Int32 { get }
var ALC_TRUE: Int32 { get }
var ALC_FREQUENCY: Int32 { get }
var ALC_REFRESH: Int32 { get }
var ALC_SYNC: Int32 { get }
var ALC_MONO_SOURCES: Int32 { get }
var ALC_STEREO_SOURCES: Int32 { get }
var ALC_NO_ERROR: Int32 { get }
var ALC_INVALID_DEVICE: Int32 { get }
var ALC_INVALID_CONTEXT: Int32 { get }
var ALC_INVALID_ENUM: Int32 { get }
var ALC_INVALID_VALUE: Int32 { get }
var ALC_OUT_OF_MEMORY: Int32 { get }
var ALC_DEFAULT_DEVICE_SPECIFIER: Int32 { get }
var ALC_DEVICE_SPECIFIER: Int32 { get }
var ALC_EXTENSIONS: Int32 { get }
var ALC_MAJOR_VERSION: Int32 { get }
var ALC_MINOR_VERSION: Int32 { get }
var ALC_ATTRIBUTES_SIZE: Int32 { get }
var ALC_ALL_ATTRIBUTES: Int32 { get }
var ALC_DEFAULT_ALL_DEVICES_SPECIFIER: Int32 { get }
var ALC_ALL_DEVICES_SPECIFIER: Int32 { get }
var ALC_CAPTURE_DEVICE_SPECIFIER: Int32 { get }
var ALC_CAPTURE_DEFAULT_DEVICE_SPECIFIER: Int32 { get }
var ALC_CAPTURE_SAMPLES: Int32 { get }
@discardableResult
func alcCreateContext(_ device: OpaquePointer!, _ attrlist: UnsafePointer<ALCint>!) -> OpaquePointer!
@discardableResult
func alcMakeContextCurrent(_ context: OpaquePointer!) -> ALCboolean
func alcProcessContext(_ context: OpaquePointer!)
func alcSuspendContext(_ context: OpaquePointer!)
func alcDestroyContext(_ context: OpaquePointer!)
@discardableResult
func alcGetCurrentContext() -> OpaquePointer!
@discardableResult
func alcGetContextsDevice(_ context: OpaquePointer!) -> OpaquePointer!
@discardableResult
func alcOpenDevice(_ devicename: UnsafePointer<ALCchar>!) -> OpaquePointer!
@discardableResult
func alcCloseDevice(_ device: OpaquePointer!) -> ALCboolean
@discardableResult
func alcGetError(_ device: OpaquePointer!) -> ALCenum
@discardableResult
func alcIsExtensionPresent(_ device: OpaquePointer!, _ extname: UnsafePointer<ALCchar>!) -> ALCboolean
@discardableResult
func alcGetProcAddress(_ device: OpaquePointer!, _ funcname: UnsafePointer<ALCchar>!) -> UnsafeMutablePointer<Void>!
@discardableResult
func alcGetEnumValue(_ device: OpaquePointer!, _ enumname: UnsafePointer<ALCchar>!) -> ALCenum
@discardableResult
func alcGetString(_ device: OpaquePointer!, _ param: ALCenum) -> UnsafePointer<ALCchar>!
func alcGetIntegerv(_ device: OpaquePointer!, _ param: ALCenum, _ size: ALCsizei, _ data: UnsafeMutablePointer<ALCint>!)
@discardableResult
func alcCaptureOpenDevice(_ devicename: UnsafePointer<ALCchar>!, _ frequency: ALCuint, _ format: ALCenum, _ buffersize: ALCsizei) -> OpaquePointer!
@discardableResult
func alcCaptureCloseDevice(_ device: OpaquePointer!) -> ALCboolean
func alcCaptureStart(_ device: OpaquePointer!)
func alcCaptureStop(_ device: OpaquePointer!)
func alcCaptureSamples(_ device: OpaquePointer!, _ buffer: UnsafeMutablePointer<Void>!, _ samples: ALCsizei)
typealias LPALCCREATECONTEXT = @convention(c) (OpaquePointer!, UnsafePointer<ALCint>!) -> OpaquePointer!
typealias LPALCMAKECONTEXTCURRENT = @convention(c) (OpaquePointer!) -> ALCboolean
typealias LPALCPROCESSCONTEXT = @convention(c) (OpaquePointer!) -> Void
typealias LPALCSUSPENDCONTEXT = @convention(c) (OpaquePointer!) -> Void
typealias LPALCDESTROYCONTEXT = @convention(c) (OpaquePointer!) -> Void
typealias LPALCGETCURRENTCONTEXT = @convention(c) () -> OpaquePointer!
typealias LPALCGETCONTEXTSDEVICE = @convention(c) (OpaquePointer!) -> OpaquePointer!
typealias LPALCOPENDEVICE = @convention(c) (UnsafePointer<ALCchar>!) -> OpaquePointer!
typealias LPALCCLOSEDEVICE = @convention(c) (OpaquePointer!) -> ALCboolean
typealias LPALCGETERROR = @convention(c) (OpaquePointer!) -> ALCenum
typealias LPALCISEXTENSIONPRESENT = @convention(c) (OpaquePointer!, UnsafePointer<ALCchar>!) -> ALCboolean
typealias LPALCGETPROCADDRESS = @convention(c) (OpaquePointer!, UnsafePointer<ALCchar>!) -> UnsafeMutablePointer<Void>!
typealias LPALCGETENUMVALUE = @convention(c) (OpaquePointer!, UnsafePointer<ALCchar>!) -> ALCenum
typealias LPALCGETSTRING = @convention(c) (OpaquePointer!, ALCenum) -> UnsafePointer<ALCchar>!
typealias LPALCGETINTEGERV = @convention(c) (OpaquePointer!, ALCenum, ALCsizei, UnsafeMutablePointer<ALCint>!) -> Void
typealias LPALCCAPTUREOPENDEVICE = @convention(c) (UnsafePointer<ALCchar>!, ALCuint, ALCenum, ALCsizei) -> OpaquePointer!
typealias LPALCCAPTURECLOSEDEVICE = @convention(c) (OpaquePointer!) -> ALCboolean
typealias LPALCCAPTURESTART = @convention(c) (OpaquePointer!) -> Void
typealias LPALCCAPTURESTOP = @convention(c) (OpaquePointer!) -> Void
typealias LPALCCAPTURESAMPLES = @convention(c) (OpaquePointer!, UnsafeMutablePointer<Void>!, ALCsizei) -> Void
