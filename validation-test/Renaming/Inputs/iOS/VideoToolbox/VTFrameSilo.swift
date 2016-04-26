
class VTFrameSilo {
}
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloGetTypeID() -> CFTypeID
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloCreate(_ allocator: CFAllocator?, _ fileURL: CFURL?, _ timeRange: CMTimeRange, _ options: CFDictionary?, _ siloOut: UnsafeMutablePointer<VTFrameSilo?>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloAddSampleBuffer(_ silo: VTFrameSilo, _ sampleBuffer: CMSampleBuffer) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloSetTimeRangesForNextPass(_ silo: VTFrameSilo, _ timeRangeCount: CMItemCount, _ timeRangeArray: UnsafePointer<CMTimeRange>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloGetProgressOfCurrentPass(_ silo: VTFrameSilo, _ progressOut: UnsafeMutablePointer<Float32>) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloCallFunctionForEachSampleBuffer(_ silo: VTFrameSilo, _ timeRange: CMTimeRange, _ callbackInfo: UnsafeMutablePointer<Void>?, _ callback: @convention(c) (UnsafeMutablePointer<Void>?, CMSampleBuffer) -> OSStatus) -> OSStatus
@available(iOS 8.0, *)
@discardableResult
func VTFrameSiloCallBlockForEachSampleBuffer(_ silo: VTFrameSilo, _ timeRange: CMTimeRange, _ handler: (CMSampleBuffer) -> OSStatus) -> OSStatus
