
class VTFrameSilo {
}
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloGetTypeID() -> CFTypeID
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloCreate(_ allocator: CFAllocator?, _ fileURL: CFURL?, _ timeRange: CMTimeRange, _ options: CFDictionary?, _ siloOut: UnsafeMutablePointer<VTFrameSilo?>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloAddSampleBuffer(_ silo: VTFrameSilo, _ sampleBuffer: CMSampleBuffer) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloSetTimeRangesForNextPass(_ silo: VTFrameSilo, _ timeRangeCount: CMItemCount, _ timeRangeArray: UnsafePointer<CMTimeRange>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloGetProgressOfCurrentPass(_ silo: VTFrameSilo, _ progressOut: UnsafeMutablePointer<Float32>) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloCallFunctionForEachSampleBuffer(_ silo: VTFrameSilo, _ timeRange: CMTimeRange, _ callbackInfo: UnsafeMutablePointer<Void>?, _ callback: @convention(c) (UnsafeMutablePointer<Void>?, CMSampleBuffer) -> OSStatus) -> OSStatus
@available(OSX 10.10, *)
@discardableResult
func VTFrameSiloCallBlockForEachSampleBuffer(_ silo: VTFrameSilo, _ timeRange: CMTimeRange, _ handler: (CMSampleBuffer) -> OSStatus) -> OSStatus
