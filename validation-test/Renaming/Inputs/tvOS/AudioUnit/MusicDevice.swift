
typealias MusicDeviceInstrumentID = UInt32
struct MusicDeviceStdNoteParams {
  var argCount: UInt32
  var mPitch: Float32
  var mVelocity: Float32
  init()
  init(argCount argCount: UInt32, mPitch mPitch: Float32, mVelocity mVelocity: Float32)
}
struct NoteParamsControlValue {
  var mID: AudioUnitParameterID
  var mValue: AudioUnitParameterValue
  init()
  init(mID mID: AudioUnitParameterID, mValue mValue: AudioUnitParameterValue)
}
struct MusicDeviceNoteParams {
  var argCount: UInt32
  var mPitch: Float32
  var mVelocity: Float32
  var mControls: (NoteParamsControlValue)
  init()
  init(argCount argCount: UInt32, mPitch mPitch: Float32, mVelocity mVelocity: Float32, mControls mControls: (NoteParamsControlValue))
}
var kMusicNoteEvent_UseGroupInstrument: UInt32 { get }
var kMusicNoteEvent_Unused: UInt32 { get }
typealias MusicDeviceGroupID = UInt32
typealias NoteInstanceID = UInt32
typealias MusicDeviceComponent = AudioComponentInstance
@available(tvOS 5.0, *)
@discardableResult
func MusicDeviceMIDIEvent(_ inUnit: MusicDeviceComponent, _ inStatus: UInt32, _ inData1: UInt32, _ inData2: UInt32, _ inOffsetSampleFrame: UInt32) -> OSStatus
@available(tvOS 5.0, *)
@discardableResult
func MusicDeviceSysEx(_ inUnit: MusicDeviceComponent, _ inData: UnsafePointer<UInt8>, _ inLength: UInt32) -> OSStatus
@available(tvOS 5.0, *)
@discardableResult
func MusicDeviceStartNote(_ inUnit: MusicDeviceComponent, _ inInstrument: MusicDeviceInstrumentID, _ inGroupID: MusicDeviceGroupID, _ outNoteInstanceID: UnsafeMutablePointer<NoteInstanceID>, _ inOffsetSampleFrame: UInt32, _ inParams: UnsafePointer<MusicDeviceNoteParams>) -> OSStatus
@available(tvOS 5.0, *)
@discardableResult
func MusicDeviceStopNote(_ inUnit: MusicDeviceComponent, _ inGroupID: MusicDeviceGroupID, _ inNoteInstanceID: NoteInstanceID, _ inOffsetSampleFrame: UInt32) -> OSStatus
var kMusicDeviceRange: Int { get }
var kMusicDeviceMIDIEventSelect: Int { get }
var kMusicDeviceSysExSelect: Int { get }
var kMusicDevicePrepareInstrumentSelect: Int { get }
var kMusicDeviceReleaseInstrumentSelect: Int { get }
var kMusicDeviceStartNoteSelect: Int { get }
var kMusicDeviceStopNoteSelect: Int { get }
typealias MusicDeviceMIDIEventProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UInt32, UInt32) -> OSStatus
typealias MusicDeviceSysExProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafePointer<UInt8>, UInt32) -> OSStatus
typealias MusicDeviceStartNoteProc = @convention(c) (UnsafeMutablePointer<Void>, MusicDeviceInstrumentID, MusicDeviceGroupID, UnsafeMutablePointer<NoteInstanceID>, UInt32, UnsafePointer<MusicDeviceNoteParams>) -> OSStatus
typealias MusicDeviceStopNoteProc = @convention(c) (UnsafeMutablePointer<Void>, MusicDeviceGroupID, NoteInstanceID, UInt32) -> OSStatus
