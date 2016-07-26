
@available(OSX 10.0, *)
@discardableResult
func AudioGetCurrentHostTime() -> UInt64
@available(OSX 10.0, *)
@discardableResult
func AudioGetHostClockFrequency() -> Float64
@available(OSX 10.0, *)
@discardableResult
func AudioGetHostClockMinimumTimeDelta() -> UInt32
@available(OSX 10.0, *)
@discardableResult
func AudioConvertHostTimeToNanos(_ inHostTime: UInt64) -> UInt64
@available(OSX 10.0, *)
@discardableResult
func AudioConvertNanosToHostTime(_ inNanos: UInt64) -> UInt64
