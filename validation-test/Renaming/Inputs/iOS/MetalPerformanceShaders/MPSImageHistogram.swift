
struct MPSImageHistogramInfo {
  var numberOfHistogramEntries: Int
  var histogramForAlpha: ObjCBool
  var minPixelValue: vector_float4
  var maxPixelValue: vector_float4
  init()
  init(numberOfHistogramEntries numberOfHistogramEntries: Int, histogramForAlpha histogramForAlpha: ObjCBool, minPixelValue minPixelValue: vector_float4, maxPixelValue maxPixelValue: vector_float4)
}
@available(iOS 9.0, *)
class MPSImageHistogram : MPSKernel {
  var clipRectSource: MTLRegion
  var zeroHistogram: Bool
  var histogramInfo: MPSImageHistogramInfo { get }
  init(device device: MTLDevice, histogramInfo histogramInfo: UnsafePointer<MPSImageHistogramInfo>)
  func encode(to commandBuffer: MTLCommandBuffer, sourceTexture source: MTLTexture, histogram histogram: MTLBuffer, histogramOffset histogramOffset: Int)
  @discardableResult
  func histogramSize(forSourceFormat sourceFormat: MTLPixelFormat) -> Int
}
@available(iOS 9.0, *)
class MPSImageHistogramEqualization : MPSUnaryImageKernel {
  var histogramInfo: MPSImageHistogramInfo { get }
  init(device device: MTLDevice, histogramInfo histogramInfo: UnsafePointer<MPSImageHistogramInfo>)
  func encodeTransform(to commandBuffer: MTLCommandBuffer, sourceTexture source: MTLTexture, histogram histogram: MTLBuffer, histogramOffset histogramOffset: Int)
}
@available(iOS 9.0, *)
class MPSImageHistogramSpecification : MPSUnaryImageKernel {
  var histogramInfo: MPSImageHistogramInfo { get }
  init(device device: MTLDevice, histogramInfo histogramInfo: UnsafePointer<MPSImageHistogramInfo>)
  func encodeTransform(to commandBuffer: MTLCommandBuffer, sourceTexture source: MTLTexture, sourceHistogram sourceHistogram: MTLBuffer, sourceHistogramOffset sourceHistogramOffset: Int, desiredHistogram desiredHistogram: MTLBuffer, desiredHistogramOffset desiredHistogramOffset: Int)
}
