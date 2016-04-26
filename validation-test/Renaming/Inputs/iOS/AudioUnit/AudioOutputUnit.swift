
@available(iOS 2.0, *)
@discardableResult
func AudioOutputUnitStart(_ ci: AudioUnit) -> OSStatus
@available(iOS 2.0, *)
@discardableResult
func AudioOutputUnitStop(_ ci: AudioUnit) -> OSStatus
var kAudioOutputUnitRange: Int { get }
var kAudioOutputUnitStartSelect: Int { get }
var kAudioOutputUnitStopSelect: Int { get }
typealias AudioOutputUnitStartProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioOutputUnitStopProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
