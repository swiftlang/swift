
typealias DWORD = UInt32
typealias LPDWORD = UnsafeMutablePointer<DWORD>
typealias LONG = Int32
typealias LPLONG = UnsafeMutablePointer<LONG>
var FF_INFINITE: UInt { get }
var FF_DEGREES: Int32 { get }
var FF_FFNOMINALMAX: Int32 { get }
var FF_SECONDS: Int32 { get }
var FFEFF_OBJECTOFFSETS: UInt { get }
var FFEFF_CARTESIAN: Int { get }
var FFEFF_POLAR: Int { get }
var FFEFF_SPHERICAL: Int { get }
typealias FFCoordinateSystemFlag = UInt32
var FFEP_DURATION: UInt32 { get }
var FFEP_SAMPLEPERIOD: UInt32 { get }
var FFEP_GAIN: UInt32 { get }
var FFEP_TRIGGERBUTTON: UInt32 { get }
var FFEP_TRIGGERREPEATINTERVAL: UInt32 { get }
var FFEP_AXES: UInt32 { get }
var FFEP_DIRECTION: UInt32 { get }
var FFEP_ENVELOPE: UInt32 { get }
var FFEP_TYPESPECIFICPARAMS: UInt32 { get }
var FFEP_STARTDELAY: UInt32 { get }
var FFEP_ALLPARAMS: UInt32 { get }
var FFEP_START: UInt32 { get }
var FFEP_NORESTART: UInt32 { get }
var FFEP_NODOWNLOAD: UInt32 { get }
var FFEB_NOTRIGGER: UInt32 { get }
typealias FFEffectParameterFlag = UInt32
var FFES_SOLO: UInt32 { get }
var FFES_NODOWNLOAD: UInt32 { get }
typealias FFEffectStartFlag = UInt32
var FFEGES_NOTPLAYING: Int { get }
var FFEGES_PLAYING: Int { get }
var FFEGES_EMULATED: Int { get }
typealias FFEffectStatusFlag = UInt32
var FFSFFC_RESET: Int { get }
var FFSFFC_STOPALL: Int { get }
var FFSFFC_PAUSE: Int { get }
var FFSFFC_CONTINUE: Int { get }
var FFSFFC_SETACTUATORSON: Int { get }
var FFSFFC_SETACTUATORSOFF: Int { get }
typealias FFCommandFlag = UInt32
var FFGFFS_EMPTY: UInt32 { get }
var FFGFFS_STOPPED: UInt32 { get }
var FFGFFS_PAUSED: UInt32 { get }
var FFGFFS_ACTUATORSON: UInt32 { get }
var FFGFFS_ACTUATORSOFF: UInt32 { get }
var FFGFFS_POWERON: UInt32 { get }
var FFGFFS_POWEROFF: UInt32 { get }
var FFGFFS_SAFETYSWITCHON: UInt32 { get }
var FFGFFS_SAFETYSWITCHOFF: UInt32 { get }
var FFGFFS_USERFFSWITCHON: UInt32 { get }
var FFGFFS_USERFFSWITCHOFF: UInt32 { get }
var FFGFFS_DEVICELOST: UInt32 { get }
typealias FFState = UInt32
var FFJOFS_X: Int32 { get }
var FFJOFS_Y: Int32 { get }
var FFJOFS_Z: Int32 { get }
var FFJOFS_RX: Int32 { get }
var FFJOFS_RY: Int32 { get }
var FFJOFS_RZ: Int32 { get }
var FFPROP_FFGAIN: Int { get }
var FFPROP_AUTOCENTER: Int { get }
typealias FFProperty = UInt32
var FFSCL_EXCLUSIVE: Int { get }
var FFSCL_NONEXCLUSIVE: Int { get }
var FFSCL_FOREGROUND: Int { get }
var FFSCL_BACKGROUND: Int { get }
typealias FFCooperativeLevelFlag = UInt32
var FFCAP_ET_CONSTANTFORCE: Int { get }
var FFCAP_ET_RAMPFORCE: Int { get }
var FFCAP_ET_SQUARE: Int { get }
var FFCAP_ET_SINE: Int { get }
var FFCAP_ET_TRIANGLE: Int { get }
var FFCAP_ET_SAWTOOTHUP: Int { get }
var FFCAP_ET_SAWTOOTHDOWN: Int { get }
var FFCAP_ET_SPRING: Int { get }
var FFCAP_ET_DAMPER: Int { get }
var FFCAP_ET_INERTIA: Int { get }
var FFCAP_ET_FRICTION: Int { get }
var FFCAP_ET_CUSTOMFORCE: Int { get }
typealias FFCapabilitiesEffectType = UInt32
var FFCAP_ST_KINESTHETIC: Int { get }
var FFCAP_ST_VIBRATION: Int { get }
typealias FFCapabilitiesEffectSubType = UInt32
var E_PENDING: Int { get }
var FFERR_DEVICEFULL: Int { get }
var FFERR_MOREDATA: Int { get }
var FFERR_NOTDOWNLOADED: Int { get }
var FFERR_HASEFFECTS: Int { get }
var FFERR_INCOMPLETEEFFECT: Int { get }
var FFERR_EFFECTPLAYING: Int { get }
var FFERR_UNPLUGGED: Int { get }
var FFERR_INVALIDDOWNLOADID: Int { get }
var FFERR_DEVICEPAUSED: Int { get }
var FFERR_INTERNAL: Int { get }
var FFERR_EFFECTTYPEMISMATCH: Int { get }
var FFERR_UNSUPPORTEDAXIS: Int { get }
var FFERR_NOTINITIALIZED: Int { get }
var FFERR_EFFECTTYPENOTSUPPORTED: Int { get }
var FFERR_DEVICERELEASED: Int { get }
