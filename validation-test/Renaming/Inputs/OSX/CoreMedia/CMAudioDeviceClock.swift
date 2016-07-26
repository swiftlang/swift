
@available(OSX 10.8, *)
@discardableResult
func CMAudioDeviceClockCreate(_ allocator: CFAllocator?, _ deviceUID: CFString?, _ clockOut: UnsafeMutablePointer<CMClock?>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMAudioDeviceClockCreateFromAudioDeviceID(_ allocator: CFAllocator?, _ deviceID: AudioDeviceID, _ clockOut: UnsafeMutablePointer<CMClock?>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMAudioDeviceClockSetAudioDeviceUID(_ clock: CMClock, _ deviceUID: CFString?) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMAudioDeviceClockSetAudioDeviceID(_ clock: CMClock, _ deviceID: AudioDeviceID) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CMAudioDeviceClockGetAudioDevice(_ clock: CMClock, _ deviceUIDOut: AutoreleasingUnsafeMutablePointer<CFString?>?, _ deviceIDOut: UnsafeMutablePointer<AudioDeviceID>?, _ trackingDefaultDeviceOut: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
