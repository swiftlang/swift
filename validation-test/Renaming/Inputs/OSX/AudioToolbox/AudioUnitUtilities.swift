
var kAUParameterListener_AnyParameter: AudioUnitParameterID { get }
enum AudioUnitEventType : UInt32 {
  init?(rawValue rawValue: UInt32)
  var rawValue: UInt32 { get }
  case parameterValueChange
  case beginParameterChangeGesture
  case endParameterChangeGesture
  case propertyChange
}
typealias AUParameterListenerRef = OpaquePointer
typealias AUEventListenerRef = AUParameterListenerRef
struct AudioUnitEvent {
  struct __Unnamed_union_mArgument {
    var mParameter: AudioUnitParameter
    var mProperty: AudioUnitProperty
    init(mParameter mParameter: AudioUnitParameter)
    init(mProperty mProperty: AudioUnitProperty)
    init()
  }
  var mEventType: AudioUnitEventType
  var mArgument: AudioUnitEvent.__Unnamed_union_mArgument
  init()
  init(mEventType mEventType: AudioUnitEventType, mArgument mArgument: AudioUnitEvent.__Unnamed_union_mArgument)
}
typealias AUParameterListenerBlock = (UnsafeMutablePointer<Void>?, UnsafePointer<AudioUnitParameter>, AudioUnitParameterValue) -> Void
typealias AUEventListenerBlock = (UnsafeMutablePointer<Void>?, UnsafePointer<AudioUnitEvent>, UInt64, AudioUnitParameterValue) -> Void
typealias AUParameterListenerProc = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?, UnsafePointer<AudioUnitParameter>, AudioUnitParameterValue) -> Void
typealias AUEventListenerProc = @convention(c) (UnsafeMutablePointer<Void>?, UnsafeMutablePointer<Void>?, UnsafePointer<AudioUnitEvent>, UInt64, AudioUnitParameterValue) -> Void
@available(OSX 10.6, *)
@discardableResult
func AUListenerCreateWithDispatchQueue(_ outListener: UnsafeMutablePointer<AUParameterListenerRef?>, _ inNotificationInterval: Float32, _ inDispatchQueue: dispatch_queue_t, _ inBlock: AUParameterListenerBlock) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUListenerCreate(_ inProc: AUParameterListenerProc, _ inUserData: UnsafeMutablePointer<Void>, _ inRunLoop: CFRunLoop?, _ inRunLoopMode: CFString?, _ inNotificationInterval: Float32, _ outListener: UnsafeMutablePointer<AUParameterListenerRef?>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUListenerDispose(_ inListener: AUParameterListenerRef) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUListenerAddParameter(_ inListener: AUParameterListenerRef, _ inObject: UnsafeMutablePointer<Void>?, _ inParameter: UnsafePointer<AudioUnitParameter>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUListenerRemoveParameter(_ inListener: AUParameterListenerRef, _ inObject: UnsafeMutablePointer<Void>?, _ inParameter: UnsafePointer<AudioUnitParameter>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUParameterSet(_ inSendingListener: AUParameterListenerRef?, _ inSendingObject: UnsafeMutablePointer<Void>?, _ inParameter: UnsafePointer<AudioUnitParameter>, _ inValue: AudioUnitParameterValue, _ inBufferOffsetInFrames: UInt32) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUParameterListenerNotify(_ inSendingListener: AUParameterListenerRef?, _ inSendingObject: UnsafeMutablePointer<Void>?, _ inParameter: UnsafePointer<AudioUnitParameter>) -> OSStatus
@available(OSX 10.6, *)
@discardableResult
func AUEventListenerCreateWithDispatchQueue(_ outListener: UnsafeMutablePointer<AUEventListenerRef?>, _ inNotificationInterval: Float32, _ inValueChangeGranularity: Float32, _ inDispatchQueue: dispatch_queue_t, _ inBlock: AUEventListenerBlock) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AUEventListenerCreate(_ inProc: AUEventListenerProc, _ inUserData: UnsafeMutablePointer<Void>?, _ inRunLoop: CFRunLoop?, _ inRunLoopMode: CFString?, _ inNotificationInterval: Float32, _ inValueChangeGranularity: Float32, _ outListener: UnsafeMutablePointer<AUEventListenerRef?>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AUEventListenerAddEventType(_ inListener: AUEventListenerRef, _ inObject: UnsafeMutablePointer<Void>?, _ inEvent: UnsafePointer<AudioUnitEvent>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AUEventListenerRemoveEventType(_ inListener: AUEventListenerRef, _ inObject: UnsafeMutablePointer<Void>?, _ inEvent: UnsafePointer<AudioUnitEvent>) -> OSStatus
@available(OSX 10.3, *)
@discardableResult
func AUEventListenerNotify(_ inSendingListener: AUEventListenerRef?, _ inSendingObject: UnsafeMutablePointer<Void>?, _ inEvent: UnsafePointer<AudioUnitEvent>) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func AUParameterValueFromLinear(_ inLinearValue: Float32, _ inParameter: UnsafePointer<AudioUnitParameter>) -> AudioUnitParameterValue
@available(OSX 10.2, *)
@discardableResult
func AUParameterValueToLinear(_ inParameterValue: AudioUnitParameterValue, _ inParameter: UnsafePointer<AudioUnitParameter>) -> Float32
@available(OSX 10.2, *)
@discardableResult
func AUParameterFormatValue(_ inParameterValue: Float64, _ inParameter: UnsafePointer<AudioUnitParameter>, _ inTextBuffer: UnsafeMutablePointer<Int8>, _ inDigits: UInt32) -> UnsafeMutablePointer<Int8>
