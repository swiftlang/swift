
var kCMFormatDescriptionError_InvalidParameter: OSStatus { get }
var kCMFormatDescriptionError_AllocationFailed: OSStatus { get }
var kCMFormatDescriptionError_ValueNotAvailable: OSStatus { get }
class CMFormatDescription {
}
typealias CMMediaType = FourCharCode
var kCMMediaType_Video: CMMediaType { get }
var kCMMediaType_Audio: CMMediaType { get }
var kCMMediaType_Muxed: CMMediaType { get }
var kCMMediaType_Text: CMMediaType { get }
var kCMMediaType_ClosedCaption: CMMediaType { get }
var kCMMediaType_Subtitle: CMMediaType { get }
var kCMMediaType_TimeCode: CMMediaType { get }
var kCMMediaType_Metadata: CMMediaType { get }
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionCreate(_ allocator: CFAllocator?, _ mediaType: CMMediaType, _ mediaSubtype: FourCharCode, _ extensions: CFDictionary?, _ descOut: UnsafeMutablePointer<CMFormatDescription?>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionGetTypeID() -> CFTypeID
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionEqual(_ desc1: CMFormatDescription?, _ desc2: CMFormatDescription?) -> Bool
@available(tvOS 4.3, *)
@discardableResult
func CMFormatDescriptionEqualIgnoringExtensionKeys(_ desc1: CMFormatDescription?, _ desc2: CMFormatDescription?, _ formatDescriptionExtensionKeysToIgnore: CFTypeRef?, _ sampleDescriptionExtensionAtomKeysToIgnore: CFTypeRef?) -> Bool
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionGetMediaType(_ desc: CMFormatDescription) -> CMMediaType
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionGetMediaSubType(_ desc: CMFormatDescription) -> FourCharCode
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionGetExtensions(_ desc: CMFormatDescription) -> CFDictionary?
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_OriginalCompressionSettings: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_SampleDescriptionExtensionAtoms: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_VerbatimSampleDescription: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_VerbatimISOSampleEntry: CFString
@available(tvOS 4.0, *)
@discardableResult
func CMFormatDescriptionGetExtension(_ desc: CMFormatDescription, _ extensionKey: CFString) -> CFPropertyList?
typealias CMAudioCodecType = FourCharCode
var kCMAudioCodecType_AAC_LCProtected: CMAudioCodecType { get }
var kCMAudioCodecType_AAC_AudibleProtected: CMAudioCodecType { get }
typealias CMAudioFormatDescription = CMFormatDescription
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionCreate(_ allocator: CFAllocator?, _ asbd: UnsafePointer<AudioStreamBasicDescription>, _ layoutSize: Int, _ layout: UnsafePointer<AudioChannelLayout>?, _ magicCookieSize: Int, _ magicCookie: UnsafePointer<Void>?, _ extensions: CFDictionary?, _ outDesc: UnsafeMutablePointer<CMAudioFormatDescription?>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetStreamBasicDescription(_ desc: CMAudioFormatDescription) -> UnsafePointer<AudioStreamBasicDescription>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetMagicCookie(_ desc: CMAudioFormatDescription, _ cookieSizeOut: UnsafeMutablePointer<Int>?) -> UnsafePointer<Void>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetChannelLayout(_ desc: CMAudioFormatDescription, _ layoutSize: UnsafeMutablePointer<Int>?) -> UnsafePointer<AudioChannelLayout>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetFormatList(_ desc: CMAudioFormatDescription, _ formatListSize: UnsafeMutablePointer<Int>?) -> UnsafePointer<AudioFormatListItem>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetRichestDecodableFormat(_ desc: CMAudioFormatDescription) -> UnsafePointer<AudioFormatListItem>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionGetMostCompatibleFormat(_ desc: CMAudioFormatDescription) -> UnsafePointer<AudioFormatListItem>?
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionCreateSummary(_ allocator: CFAllocator?, _ formatDescriptionArray: CFArray, _ flags: UInt32, _ summaryFormatDescriptionOut: UnsafeMutablePointer<CMAudioFormatDescription?>) -> OSStatus
typealias CMAudioFormatDescriptionMask = UInt32
var kCMAudioFormatDescriptionMask_StreamBasicDescription: CMAudioFormatDescriptionMask { get }
var kCMAudioFormatDescriptionMask_MagicCookie: CMAudioFormatDescriptionMask { get }
var kCMAudioFormatDescriptionMask_ChannelLayout: CMAudioFormatDescriptionMask { get }
var kCMAudioFormatDescriptionMask_Extensions: CMAudioFormatDescriptionMask { get }
var kCMAudioFormatDescriptionMask_All: CMAudioFormatDescriptionMask { get }
@available(tvOS 4.0, *)
@discardableResult
func CMAudioFormatDescriptionEqual(_ desc1: CMAudioFormatDescription, _ desc2: CMAudioFormatDescription, _ equalityMask: CMAudioFormatDescriptionMask, _ equalityMaskOut: UnsafeMutablePointer<CMAudioFormatDescriptionMask>?) -> Bool
typealias CMVideoFormatDescription = CMFormatDescription
typealias CMPixelFormatType = FourCharCode
var kCMPixelFormat_32ARGB: CMPixelFormatType { get }
var kCMPixelFormat_32BGRA: CMPixelFormatType { get }
var kCMPixelFormat_24RGB: CMPixelFormatType { get }
var kCMPixelFormat_16BE555: CMPixelFormatType { get }
var kCMPixelFormat_16BE565: CMPixelFormatType { get }
var kCMPixelFormat_16LE555: CMPixelFormatType { get }
var kCMPixelFormat_16LE565: CMPixelFormatType { get }
var kCMPixelFormat_16LE5551: CMPixelFormatType { get }
var kCMPixelFormat_422YpCbCr8: CMPixelFormatType { get }
var kCMPixelFormat_422YpCbCr8_yuvs: CMPixelFormatType { get }
var kCMPixelFormat_444YpCbCr8: CMPixelFormatType { get }
var kCMPixelFormat_4444YpCbCrA8: CMPixelFormatType { get }
var kCMPixelFormat_422YpCbCr16: CMPixelFormatType { get }
var kCMPixelFormat_422YpCbCr10: CMPixelFormatType { get }
var kCMPixelFormat_444YpCbCr10: CMPixelFormatType { get }
var kCMPixelFormat_8IndexedGray_WhiteIsZero: CMPixelFormatType { get }
typealias CMVideoCodecType = FourCharCode
var kCMVideoCodecType_422YpCbCr8: CMVideoCodecType { get }
var kCMVideoCodecType_Animation: CMVideoCodecType { get }
var kCMVideoCodecType_Cinepak: CMVideoCodecType { get }
var kCMVideoCodecType_JPEG: CMVideoCodecType { get }
var kCMVideoCodecType_JPEG_OpenDML: CMVideoCodecType { get }
var kCMVideoCodecType_SorensonVideo: CMVideoCodecType { get }
var kCMVideoCodecType_SorensonVideo3: CMVideoCodecType { get }
var kCMVideoCodecType_H263: CMVideoCodecType { get }
var kCMVideoCodecType_H264: CMVideoCodecType { get }
var kCMVideoCodecType_HEVC: CMVideoCodecType { get }
var kCMVideoCodecType_MPEG4Video: CMVideoCodecType { get }
var kCMVideoCodecType_MPEG2Video: CMVideoCodecType { get }
var kCMVideoCodecType_MPEG1Video: CMVideoCodecType { get }
var kCMVideoCodecType_DVCNTSC: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPAL: CMVideoCodecType { get }
var kCMVideoCodecType_DVCProPAL: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPro50NTSC: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPro50PAL: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD720p60: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD720p50: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD1080i60: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD1080i50: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD1080p30: CMVideoCodecType { get }
var kCMVideoCodecType_DVCPROHD1080p25: CMVideoCodecType { get }
var kCMVideoCodecType_AppleProRes4444: CMVideoCodecType { get }
var kCMVideoCodecType_AppleProRes422HQ: CMVideoCodecType { get }
var kCMVideoCodecType_AppleProRes422: CMVideoCodecType { get }
var kCMVideoCodecType_AppleProRes422LT: CMVideoCodecType { get }
var kCMVideoCodecType_AppleProRes422Proxy: CMVideoCodecType { get }
struct CMVideoDimensions {
  var width: Int32
  var height: Int32
  init()
  init(width width: Int32, height height: Int32)
}
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_FormatName: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_Depth: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_CleanAperture: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_CleanApertureWidth: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_CleanApertureHeight: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_CleanApertureHorizontalOffset: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_CleanApertureVerticalOffset: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionKey_CleanApertureWidthRational: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionKey_CleanApertureHeightRational: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionKey_CleanApertureHorizontalOffsetRational: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionKey_CleanApertureVerticalOffsetRational: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_FieldCount: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_FieldDetail: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionFieldDetail_TemporalTopFirst: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionFieldDetail_TemporalBottomFirst: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionFieldDetail_SpatialFirstLineEarly: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionFieldDetail_SpatialFirstLineLate: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_PixelAspectRatio: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_PixelAspectRatioHorizontalSpacing: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionKey_PixelAspectRatioVerticalSpacing: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_ColorPrimaries: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_ITU_R_709_2: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_EBU_3213: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_SMPTE_C: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_DCI_P3: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_P3_D65: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionColorPrimaries_ITU_R_2020: CFString
@available(tvOS 6.0, *)
let kCMFormatDescriptionColorPrimaries_P22: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_TransferFunction: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionTransferFunction_ITU_R_709_2: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionTransferFunction_SMPTE_240M_1995: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionTransferFunction_UseGamma: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionTransferFunction_ITU_R_2020: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_GammaLevel: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_YCbCrMatrix: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionYCbCrMatrix_ITU_R_709_2: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionYCbCrMatrix_ITU_R_601_4: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionYCbCrMatrix_SMPTE_240M_1995: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionYCbCrMatrix_ITU_R_2020: CFString
@available(tvOS 4.3, *)
let kCMFormatDescriptionExtension_FullRangeVideo: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_ICCProfile: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_BytesPerRow: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_ChromaLocationTopField: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_ChromaLocationBottomField: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_Left: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_Center: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_TopLeft: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_Top: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_BottomLeft: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_Bottom: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionChromaLocation_DV420: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionConformsToMPEG2VideoProfile: CFString
var kCMMPEG2VideoProfile_HDV_720p30: Int32 { get }
var kCMMPEG2VideoProfile_HDV_1080i60: Int32 { get }
var kCMMPEG2VideoProfile_HDV_1080i50: Int32 { get }
var kCMMPEG2VideoProfile_HDV_720p24: Int32 { get }
var kCMMPEG2VideoProfile_HDV_720p25: Int32 { get }
var kCMMPEG2VideoProfile_HDV_1080p24: Int32 { get }
var kCMMPEG2VideoProfile_HDV_1080p25: Int32 { get }
var kCMMPEG2VideoProfile_HDV_1080p30: Int32 { get }
var kCMMPEG2VideoProfile_HDV_720p60: Int32 { get }
var kCMMPEG2VideoProfile_HDV_720p50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_1080i60_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_1080i50_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_1080p24_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_1080p25_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_1080p30_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_720p24_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_720p25_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_720p30_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_720p50_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_720p60_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_1080i60_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_1080i50_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_1080p24_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_1080p25_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_EX_1080p30_VBR35: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_720p50_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_720p60_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_1080i60_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_1080i50_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_1080p24_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_1080p25_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_1080p30_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD_540p: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_540p: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_720p24_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_720p25_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XDCAM_HD422_720p30_CBR50: Int32 { get }
var kCMMPEG2VideoProfile_XF: Int32 { get }
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_TemporalQuality: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_SpatialQuality: CFString
@available(tvOS 9.0, *)
let kCMFormatDescriptionExtension_VerbatimImageDescription: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_Version: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_RevisionLevel: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtension_Vendor: CFString
@available(tvOS 4.0, *)
let kCMFormatDescriptionVendor_Apple: CFString
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionCreate(_ allocator: CFAllocator?, _ codecType: CMVideoCodecType, _ width: Int32, _ height: Int32, _ extensions: CFDictionary?, _ outDesc: UnsafeMutablePointer<CMVideoFormatDescription?>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionCreateForImageBuffer(_ allocator: CFAllocator?, _ imageBuffer: CVImageBuffer, _ outDesc: UnsafeMutablePointer<CMVideoFormatDescription?>) -> OSStatus
@available(tvOS 7.0, *)
@discardableResult
func CMVideoFormatDescriptionCreateFromH264ParameterSets(_ allocator: CFAllocator?, _ parameterSetCount: Int, _ parameterSetPointers: UnsafePointer<UnsafePointer<UInt8>>, _ parameterSetSizes: UnsafePointer<Int>, _ NALUnitHeaderLength: Int32, _ formatDescriptionOut: UnsafeMutablePointer<CMFormatDescription?>) -> OSStatus
@available(tvOS 7.0, *)
@discardableResult
func CMVideoFormatDescriptionGetH264ParameterSetAtIndex(_ videoDesc: CMFormatDescription, _ parameterSetIndex: Int, _ parameterSetPointerOut: UnsafeMutablePointer<UnsafePointer<UInt8>?>?, _ parameterSetSizeOut: UnsafeMutablePointer<Int>?, _ parameterSetCountOut: UnsafeMutablePointer<Int>?, _ NALUnitHeaderLengthOut: UnsafeMutablePointer<Int32>?) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionGetDimensions(_ videoDesc: CMVideoFormatDescription) -> CMVideoDimensions
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionGetPresentationDimensions(_ videoDesc: CMVideoFormatDescription, _ usePixelAspectRatio: Bool, _ useCleanAperture: Bool) -> CGSize
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionGetCleanAperture(_ videoDesc: CMVideoFormatDescription, _ originIsAtTopLeft: Bool) -> CGRect
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionGetExtensionKeysCommonWithImageBuffers() -> CFArray
@available(tvOS 4.0, *)
@discardableResult
func CMVideoFormatDescriptionMatchesImageBuffer(_ desc: CMVideoFormatDescription, _ imageBuffer: CVImageBuffer) -> Bool
typealias CMMuxedFormatDescription = CMFormatDescription
typealias CMMuxedStreamType = FourCharCode
var kCMMuxedStreamType_MPEG1System: CMMuxedStreamType { get }
var kCMMuxedStreamType_MPEG2Transport: CMMuxedStreamType { get }
var kCMMuxedStreamType_MPEG2Program: CMMuxedStreamType { get }
var kCMMuxedStreamType_DV: CMMuxedStreamType { get }
@available(tvOS 4.0, *)
@discardableResult
func CMMuxedFormatDescriptionCreate(_ allocator: CFAllocator?, _ muxType: CMMuxedStreamType, _ extensions: CFDictionary?, _ outDesc: UnsafeMutablePointer<CMMuxedFormatDescription?>) -> OSStatus
typealias CMClosedCaptionFormatDescription = CMFormatDescription
typealias CMClosedCaptionFormatType = FourCharCode
var kCMClosedCaptionFormatType_CEA608: CMClosedCaptionFormatType { get }
var kCMClosedCaptionFormatType_CEA708: CMClosedCaptionFormatType { get }
var kCMClosedCaptionFormatType_ATSC: CMClosedCaptionFormatType { get }
typealias CMTextFormatDescription = CMFormatDescription
typealias CMTextFormatType = FourCharCode
var kCMTextFormatType_QTText: CMTextFormatType { get }
var kCMTextFormatType_3GText: CMTextFormatType { get }
typealias CMTextDisplayFlags = UInt32
var kCMTextDisplayFlag_scrollIn: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollOut: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollDirectionMask: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollDirection_bottomToTop: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollDirection_rightToLeft: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollDirection_topToBottom: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_scrollDirection_leftToRight: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_continuousKaraoke: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_writeTextVertically: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_fillTextRegion: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_obeySubtitleFormatting: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_forcedSubtitlesPresent: CMTextDisplayFlags { get }
var kCMTextDisplayFlag_allSubtitlesForced: CMTextDisplayFlags { get }
typealias CMTextJustificationValue = Int8
var kCMTextJustification_left_top: CMTextJustificationValue { get }
var kCMTextJustification_centered: CMTextJustificationValue { get }
var kCMTextJustification_bottom_right: CMTextJustificationValue { get }
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_DisplayFlags: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_BackgroundColor: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionColor_Red: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionColor_Green: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionColor_Blue: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionColor_Alpha: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_DefaultTextBox: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionRect_Top: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionRect_Left: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionRect_Bottom: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionRect_Right: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_DefaultStyle: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_StartChar: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_Font: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_FontFace: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_ForegroundColor: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_FontSize: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_HorizontalJustification: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_VerticalJustification: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_EndChar: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_FontTable: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_TextJustification: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_Height: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionStyle_Ascent: CFString
@available(tvOS 4.0, *)
let kCMTextFormatDescriptionExtension_DefaultFontName: CFString
@available(tvOS 4.0, *)
@discardableResult
func CMTextFormatDescriptionGetDisplayFlags(_ desc: CMFormatDescription, _ outDisplayFlags: UnsafeMutablePointer<CMTextDisplayFlags>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMTextFormatDescriptionGetJustification(_ desc: CMFormatDescription, _ outHorizontalJust: UnsafeMutablePointer<CMTextJustificationValue>?, _ outVerticalJust: UnsafeMutablePointer<CMTextJustificationValue>?) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMTextFormatDescriptionGetDefaultTextBox(_ desc: CMFormatDescription, _ originIsAtTopLeft: Bool, _ heightOfTextTrack: CGFloat, _ outDefaultTextBox: UnsafeMutablePointer<CGRect>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMTextFormatDescriptionGetDefaultStyle(_ desc: CMFormatDescription, _ outLocalFontID: UnsafeMutablePointer<UInt16>?, _ outBold: UnsafeMutablePointer<DarwinBoolean>?, _ outItalic: UnsafeMutablePointer<DarwinBoolean>?, _ outUnderline: UnsafeMutablePointer<DarwinBoolean>?, _ outFontSize: UnsafeMutablePointer<CGFloat>?, _ outColorComponents: UnsafeMutablePointer<CGFloat>!) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMTextFormatDescriptionGetFontName(_ desc: CMFormatDescription, _ localFontID: UInt16, _ outFontName: AutoreleasingUnsafeMutablePointer<CFString?>) -> OSStatus
typealias CMSubtitleFormatType = FourCharCode
var kCMSubtitleFormatType_3GText: CMSubtitleFormatType { get }
var kCMSubtitleFormatType_WebVTT: CMSubtitleFormatType { get }
typealias CMTimeCodeFormatDescription = CMFormatDescription
typealias CMTimeCodeFormatType = FourCharCode
var kCMTimeCodeFormatType_TimeCode32: CMTimeCodeFormatType { get }
var kCMTimeCodeFormatType_TimeCode64: CMTimeCodeFormatType { get }
var kCMTimeCodeFormatType_Counter32: CMTimeCodeFormatType { get }
var kCMTimeCodeFormatType_Counter64: CMTimeCodeFormatType { get }
var kCMTimeCodeFlag_DropFrame: UInt32 { get }
var kCMTimeCodeFlag_24HourMax: UInt32 { get }
var kCMTimeCodeFlag_NegTimesOK: UInt32 { get }
@available(tvOS 4.0, *)
@discardableResult
func CMTimeCodeFormatDescriptionCreate(_ allocator: CFAllocator?, _ timeCodeFormatType: CMTimeCodeFormatType, _ frameDuration: CMTime, _ frameQuanta: UInt32, _ tcFlags: UInt32, _ extensions: CFDictionary?, _ descOut: UnsafeMutablePointer<CMTimeCodeFormatDescription?>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMTimeCodeFormatDescriptionGetFrameDuration(_ timeCodeFormatDescription: CMTimeCodeFormatDescription) -> CMTime
@available(tvOS 4.0, *)
@discardableResult
func CMTimeCodeFormatDescriptionGetFrameQuanta(_ timeCodeFormatDescription: CMTimeCodeFormatDescription) -> UInt32
@available(tvOS 4.0, *)
@discardableResult
func CMTimeCodeFormatDescriptionGetTimeCodeFlags(_ desc: CMTimeCodeFormatDescription) -> UInt32
@available(tvOS 4.0, *)
let kCMTimeCodeFormatDescriptionExtension_SourceReferenceName: CFString
@available(tvOS 4.0, *)
let kCMTimeCodeFormatDescriptionKey_Value: CFString
@available(tvOS 4.0, *)
let kCMTimeCodeFormatDescriptionKey_LangCode: CFString
typealias CMMetadataFormatDescription = CMFormatDescription
typealias CMMetadataFormatType = FourCharCode
var kCMMetadataFormatType_ICY: CMMetadataFormatType { get }
var kCMMetadataFormatType_ID3: CMMetadataFormatType { get }
var kCMMetadataFormatType_Boxed: CMMetadataFormatType { get }
@available(tvOS 4.0, *)
let kCMFormatDescriptionExtensionKey_MetadataKeyTable: CFString
@available(tvOS 4.0, *)
let kCMMetadataFormatDescriptionKey_Namespace: CFString
@available(tvOS 4.0, *)
let kCMMetadataFormatDescriptionKey_Value: CFString
@available(tvOS 4.0, *)
let kCMMetadataFormatDescriptionKey_LocalID: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionKey_DataType: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionKey_DataTypeNamespace: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionKey_ConformingDataTypes: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionKey_LanguageTag: CFString
@available(tvOS 9.0, *)
let kCMMetadataFormatDescriptionKey_StructuralDependency: CFString
@available(tvOS 9.0, *)
let kCMMetadataFormatDescription_StructuralDependencyKey_DependencyIsInvalidFlag: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionMetadataSpecificationKey_Identifier: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionMetadataSpecificationKey_DataType: CFString
@available(tvOS 8.0, *)
let kCMMetadataFormatDescriptionMetadataSpecificationKey_ExtendedLanguageTag: CFString
@available(tvOS 9.0, *)
let kCMMetadataFormatDescriptionMetadataSpecificationKey_StructuralDependency: CFString
@available(tvOS 4.0, *)
@discardableResult
func CMMetadataFormatDescriptionCreateWithKeys(_ allocator: CFAllocator?, _ metadataType: CMMetadataFormatType, _ keys: CFArray?, _ outDesc: UnsafeMutablePointer<CMMetadataFormatDescription?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataFormatDescriptionCreateWithMetadataSpecifications(_ allocator: CFAllocator?, _ metadataType: CMMetadataFormatType, _ metadataSpecifications: CFArray, _ outDesc: UnsafeMutablePointer<CMMetadataFormatDescription?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataFormatDescriptionCreateWithMetadataFormatDescriptionAndMetadataSpecifications(_ allocator: CFAllocator?, _ srcDesc: CMMetadataFormatDescription, _ metadataSpecifications: CFArray, _ outDesc: UnsafeMutablePointer<CMMetadataFormatDescription?>) -> OSStatus
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataFormatDescriptionCreateByMergingMetadataFormatDescriptions(_ allocator: CFAllocator?, _ srcDesc1: CMMetadataFormatDescription, _ srcDesc2: CMMetadataFormatDescription, _ outDesc: UnsafeMutablePointer<CMMetadataFormatDescription?>) -> OSStatus
@available(tvOS 4.0, *)
@discardableResult
func CMMetadataFormatDescriptionGetKeyWithLocalID(_ desc: CMMetadataFormatDescription, _ localKeyID: OSType) -> CFDictionary?
@available(tvOS 8.0, *)
@discardableResult
func CMMetadataFormatDescriptionGetIdentifiers(_ desc: CMMetadataFormatDescription) -> CFArray?
