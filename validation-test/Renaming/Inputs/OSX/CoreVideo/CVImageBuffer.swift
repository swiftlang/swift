
@available(OSX 10.4, *)
let kCVImageBufferCGColorSpaceKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferCleanApertureKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferCleanApertureWidthKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferCleanApertureHeightKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferCleanApertureHorizontalOffsetKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferCleanApertureVerticalOffsetKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferPreferredCleanApertureKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldCountKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldDetailKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldDetailTemporalTopFirst: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldDetailTemporalBottomFirst: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldDetailSpatialFirstLineEarly: CFString
@available(OSX 10.4, *)
let kCVImageBufferFieldDetailSpatialFirstLineLate: CFString
@available(OSX 10.4, *)
let kCVImageBufferPixelAspectRatioKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferPixelAspectRatioHorizontalSpacingKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferPixelAspectRatioVerticalSpacingKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferDisplayDimensionsKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferDisplayWidthKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferDisplayHeightKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferGammaLevelKey: CFString
@available(OSX 10.6, *)
let kCVImageBufferICCProfileKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferYCbCrMatrixKey: CFString
@available(OSX 10.4, *)
let kCVImageBufferYCbCrMatrix_ITU_R_709_2: CFString
@available(OSX 10.4, *)
let kCVImageBufferYCbCrMatrix_ITU_R_601_4: CFString
@available(OSX 10.4, *)
let kCVImageBufferYCbCrMatrix_SMPTE_240M_1995: CFString
@available(OSX 10.11, *)
let kCVImageBufferYCbCrMatrix_ITU_R_2020: CFString
@available(OSX 10.5, *)
let kCVImageBufferColorPrimariesKey: CFString
@available(OSX 10.5, *)
let kCVImageBufferColorPrimaries_ITU_R_709_2: CFString
@available(OSX 10.5, *)
let kCVImageBufferColorPrimaries_EBU_3213: CFString
@available(OSX 10.5, *)
let kCVImageBufferColorPrimaries_SMPTE_C: CFString
@available(OSX 10.8, *)
let kCVImageBufferColorPrimaries_P22: CFString
@available(OSX 10.11, *)
let kCVImageBufferColorPrimaries_DCI_P3: CFString
@available(OSX 10.11, *)
let kCVImageBufferColorPrimaries_P3_D65: CFString
@available(OSX 10.11, *)
let kCVImageBufferColorPrimaries_ITU_R_2020: CFString
@available(OSX 10.5, *)
let kCVImageBufferTransferFunctionKey: CFString
@available(OSX 10.5, *)
let kCVImageBufferTransferFunction_ITU_R_709_2: CFString
@available(OSX 10.6, *)
let kCVImageBufferTransferFunction_SMPTE_240M_1995: CFString
@available(OSX 10.6, *)
let kCVImageBufferTransferFunction_UseGamma: CFString
@available(OSX 10.11, *)
let kCVImageBufferTransferFunction_ITU_R_2020: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocationTopFieldKey: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocationBottomFieldKey: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_Left: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_Center: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_TopLeft: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_Top: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_BottomLeft: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_Bottom: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaLocation_DV420: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaSubsamplingKey: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaSubsampling_420: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaSubsampling_422: CFString
@available(OSX 10.5, *)
let kCVImageBufferChromaSubsampling_411: CFString
@available(OSX 10.10, *)
let kCVImageBufferAlphaChannelIsOpaque: CFString
typealias CVImageBuffer = CVBuffer
@available(OSX 10.4, *)
@discardableResult
func CVImageBufferGetEncodedSize(_ imageBuffer: CVImageBuffer) -> CGSize
@available(OSX 10.4, *)
@discardableResult
func CVImageBufferGetDisplaySize(_ imageBuffer: CVImageBuffer) -> CGSize
@available(OSX 10.4, *)
@discardableResult
func CVImageBufferGetCleanRect(_ imageBuffer: CVImageBuffer) -> CGRect
@available(OSX 10.4, *)
@discardableResult
func CVImageBufferIsFlipped(_ imageBuffer: CVImageBuffer) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CVImageBufferGetColorSpace(_ imageBuffer: CVImageBuffer) -> Unmanaged<CGColorSpace>?
@available(OSX 10.8, *)
@discardableResult
func CVImageBufferCreateColorSpaceFromAttachments(_ attachments: CFDictionary) -> Unmanaged<CGColorSpace>?
