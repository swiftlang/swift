
var kFFAPIMajorRev: Int { get }
var kFFAPIMinorAndBugRev: Int { get }
var kFFAPIStage: Int { get }
var kFFAPINonRelRev: Int { get }
struct FFCONSTANTFORCE {
  var lMagnitude: LONG
  init()
  init(lMagnitude lMagnitude: LONG)
}
typealias PFFCONSTANTFORCE = UnsafeMutablePointer<FFCONSTANTFORCE>
struct FFRAMPFORCE {
  var lStart: LONG
  var lEnd: LONG
  init()
  init(lStart lStart: LONG, lEnd lEnd: LONG)
}
typealias PFFRAMPFORCE = UnsafeMutablePointer<FFRAMPFORCE>
struct FFPERIODIC {
  var dwMagnitude: DWORD
  var lOffset: LONG
  var dwPhase: DWORD
  var dwPeriod: DWORD
  init()
  init(dwMagnitude dwMagnitude: DWORD, lOffset lOffset: LONG, dwPhase dwPhase: DWORD, dwPeriod dwPeriod: DWORD)
}
typealias PFFPERIODIC = UnsafeMutablePointer<FFPERIODIC>
struct FFCONDITION {
  var lOffset: LONG
  var lPositiveCoefficient: LONG
  var lNegativeCoefficient: LONG
  var dwPositiveSaturation: DWORD
  var dwNegativeSaturation: DWORD
  var lDeadBand: LONG
  init()
  init(lOffset lOffset: LONG, lPositiveCoefficient lPositiveCoefficient: LONG, lNegativeCoefficient lNegativeCoefficient: LONG, dwPositiveSaturation dwPositiveSaturation: DWORD, dwNegativeSaturation dwNegativeSaturation: DWORD, lDeadBand lDeadBand: LONG)
}
typealias PFFCONDITION = UnsafeMutablePointer<FFCONDITION>
struct FFCUSTOMFORCE {
  var cChannels: DWORD
  var dwSamplePeriod: DWORD
  var cSamples: DWORD
  var rglForceData: LPLONG!
  init()
  init(cChannels cChannels: DWORD, dwSamplePeriod dwSamplePeriod: DWORD, cSamples cSamples: DWORD, rglForceData rglForceData: LPLONG!)
}
typealias PFFCUSTOMFORCE = UnsafeMutablePointer<FFCUSTOMFORCE>
struct FFENVELOPE {
  var dwSize: DWORD
  var dwAttackLevel: DWORD
  var dwAttackTime: DWORD
  var dwFadeLevel: DWORD
  var dwFadeTime: DWORD
  init()
  init(dwSize dwSize: DWORD, dwAttackLevel dwAttackLevel: DWORD, dwAttackTime dwAttackTime: DWORD, dwFadeLevel dwFadeLevel: DWORD, dwFadeTime dwFadeTime: DWORD)
}
typealias PFFENVELOPE = UnsafeMutablePointer<FFENVELOPE>
struct FFEFFECT {
  var dwSize: DWORD
  var dwFlags: DWORD
  var dwDuration: DWORD
  var dwSamplePeriod: DWORD
  var dwGain: DWORD
  var dwTriggerButton: DWORD
  var dwTriggerRepeatInterval: DWORD
  var cAxes: DWORD
  var rgdwAxes: LPDWORD!
  var rglDirection: LPLONG!
  var lpEnvelope: PFFENVELOPE!
  var cbTypeSpecificParams: DWORD
  var lpvTypeSpecificParams: UnsafeMutablePointer<Void>!
  var dwStartDelay: DWORD
  init()
  init(dwSize dwSize: DWORD, dwFlags dwFlags: DWORD, dwDuration dwDuration: DWORD, dwSamplePeriod dwSamplePeriod: DWORD, dwGain dwGain: DWORD, dwTriggerButton dwTriggerButton: DWORD, dwTriggerRepeatInterval dwTriggerRepeatInterval: DWORD, cAxes cAxes: DWORD, rgdwAxes rgdwAxes: LPDWORD!, rglDirection rglDirection: LPLONG!, lpEnvelope lpEnvelope: PFFENVELOPE!, cbTypeSpecificParams cbTypeSpecificParams: DWORD, lpvTypeSpecificParams lpvTypeSpecificParams: UnsafeMutablePointer<Void>!, dwStartDelay dwStartDelay: DWORD)
}
typealias PFFEFFECT = UnsafeMutablePointer<FFEFFECT>
struct FFEFFESCAPE {
  var dwSize: DWORD
  var dwCommand: DWORD
  var lpvInBuffer: UnsafeMutablePointer<Void>!
  var cbInBuffer: DWORD
  var lpvOutBuffer: UnsafeMutablePointer<Void>!
  var cbOutBuffer: DWORD
  init()
  init(dwSize dwSize: DWORD, dwCommand dwCommand: DWORD, lpvInBuffer lpvInBuffer: UnsafeMutablePointer<Void>!, cbInBuffer cbInBuffer: DWORD, lpvOutBuffer lpvOutBuffer: UnsafeMutablePointer<Void>!, cbOutBuffer cbOutBuffer: DWORD)
}
typealias PFFEFFESCAPE = UnsafeMutablePointer<FFEFFESCAPE>
struct FFCAPABILITIES {
  var ffSpecVer: NumVersion
  var supportedEffects: UInt32
  var emulatedEffects: UInt32
  var subType: UInt32
  var numFfAxes: UInt32
  var ffAxes: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
  var storageCapacity: UInt32
  var playbackCapacity: UInt32
  var firmwareVer: NumVersion
  var hardwareVer: NumVersion
  var driverVer: NumVersion
  init()
  init(ffSpecVer ffSpecVer: NumVersion, supportedEffects supportedEffects: UInt32, emulatedEffects emulatedEffects: UInt32, subType subType: UInt32, numFfAxes numFfAxes: UInt32, ffAxes ffAxes: (UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8), storageCapacity storageCapacity: UInt32, playbackCapacity playbackCapacity: UInt32, firmwareVer firmwareVer: NumVersion, hardwareVer hardwareVer: NumVersion, driverVer driverVer: NumVersion)
}
typealias PFFCAPABILITIES = UnsafeMutablePointer<FFCAPABILITIES>
struct __FFDHIDDEN {
  init()
}
typealias FFDeviceObjectReference = UnsafeMutablePointer<__FFDHIDDEN>
struct __FFEHIDDEN {
  init()
}
typealias FFEffectObjectReference = UnsafeMutablePointer<__FFEHIDDEN>
@discardableResult
func FFCreateDevice(_ hidDevice: io_service_t, _ pDeviceReference: UnsafeMutablePointer<FFDeviceObjectReference?>!) -> HRESULT
@discardableResult
func FFReleaseDevice(_ deviceReference: FFDeviceObjectReference!) -> HRESULT
@discardableResult
func FFIsForceFeedback(_ hidDevice: io_service_t) -> HRESULT
@discardableResult
func FFDeviceCreateEffect(_ deviceReference: FFDeviceObjectReference!, _ uuidRef: CFUUID!, _ pEffectDefinition: UnsafeMutablePointer<FFEFFECT>!, _ pEffectReference: UnsafeMutablePointer<FFEffectObjectReference?>!) -> HRESULT
@discardableResult
func FFDeviceReleaseEffect(_ deviceReference: FFDeviceObjectReference!, _ effectReference: FFEffectObjectReference!) -> HRESULT
@discardableResult
func FFDeviceEscape(_ deviceReference: FFDeviceObjectReference!, _ pFFEffectEscape: UnsafeMutablePointer<FFEFFESCAPE>!) -> HRESULT
@discardableResult
func FFDeviceGetForceFeedbackState(_ deviceReference: FFDeviceObjectReference!, _ pFFState: UnsafeMutablePointer<FFState>!) -> HRESULT
@discardableResult
func FFDeviceSendForceFeedbackCommand(_ deviceReference: FFDeviceObjectReference!, _ flags: FFCommandFlag) -> HRESULT
@discardableResult
func FFDeviceSetForceFeedbackProperty(_ deviceReference: FFDeviceObjectReference!, _ property: FFProperty, _ pValue: UnsafeMutablePointer<Void>!) -> HRESULT
@discardableResult
func FFDeviceGetForceFeedbackProperty(_ deviceReference: FFDeviceObjectReference!, _ property: FFProperty, _ pValue: UnsafeMutablePointer<Void>!, _ valueSize: IOByteCount) -> HRESULT
@discardableResult
func FFDeviceSetCooperativeLevel(_ deviceReference: FFDeviceObjectReference!, _ taskIdentifier: UnsafeMutablePointer<Void>!, _ flags: FFCooperativeLevelFlag) -> HRESULT
@discardableResult
func FFDeviceGetForceFeedbackCapabilities(_ deviceReference: FFDeviceObjectReference!, _ pFFCapabilities: UnsafeMutablePointer<FFCAPABILITIES>!) -> HRESULT
@discardableResult
func FFEffectDownload(_ effectReference: FFEffectObjectReference!) -> HRESULT
@discardableResult
func FFEffectEscape(_ effectReference: FFEffectObjectReference!, _ pFFEffectEscape: UnsafeMutablePointer<FFEFFESCAPE>!) -> HRESULT
@discardableResult
func FFEffectGetEffectStatus(_ effectReference: FFEffectObjectReference!, _ pFlags: UnsafeMutablePointer<FFEffectStatusFlag>!) -> HRESULT
@discardableResult
func FFEffectGetParameters(_ effectReference: FFEffectObjectReference!, _ pFFEffect: UnsafeMutablePointer<FFEFFECT>!, _ flags: FFEffectParameterFlag) -> HRESULT
@discardableResult
func FFEffectSetParameters(_ effectReference: FFEffectObjectReference!, _ pFFEffect: UnsafeMutablePointer<FFEFFECT>!, _ flags: FFEffectParameterFlag) -> HRESULT
@discardableResult
func FFEffectStart(_ effectReference: FFEffectObjectReference!, _ iterations: UInt32, _ flags: FFEffectStartFlag) -> HRESULT
@discardableResult
func FFEffectStop(_ effectReference: FFEffectObjectReference!) -> HRESULT
@discardableResult
func FFEffectUnload(_ effectReference: FFEffectObjectReference!) -> HRESULT
