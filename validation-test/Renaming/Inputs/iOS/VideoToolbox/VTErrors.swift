
var kVTPropertyNotSupportedErr: OSStatus { get }
var kVTPropertyReadOnlyErr: OSStatus { get }
var kVTParameterErr: OSStatus { get }
var kVTInvalidSessionErr: OSStatus { get }
var kVTAllocationFailedErr: OSStatus { get }
var kVTPixelTransferNotSupportedErr: OSStatus { get }
var kVTCouldNotFindVideoDecoderErr: OSStatus { get }
var kVTCouldNotCreateInstanceErr: OSStatus { get }
var kVTCouldNotFindVideoEncoderErr: OSStatus { get }
var kVTVideoDecoderBadDataErr: OSStatus { get }
var kVTVideoDecoderUnsupportedDataFormatErr: OSStatus { get }
var kVTVideoDecoderMalfunctionErr: OSStatus { get }
var kVTVideoEncoderMalfunctionErr: OSStatus { get }
var kVTVideoDecoderNotAvailableNowErr: OSStatus { get }
var kVTImageRotationNotSupportedErr: OSStatus { get }
var kVTVideoEncoderNotAvailableNowErr: OSStatus { get }
var kVTFormatDescriptionChangeNotSupportedErr: OSStatus { get }
var kVTInsufficientSourceColorDataErr: OSStatus { get }
var kVTCouldNotCreateColorCorrectionDataErr: OSStatus { get }
var kVTColorSyncTransformConvertFailedErr: OSStatus { get }
var kVTVideoDecoderAuthorizationErr: OSStatus { get }
var kVTVideoEncoderAuthorizationErr: OSStatus { get }
var kVTColorCorrectionPixelTransferFailedErr: OSStatus { get }
var kVTMultiPassStorageIdentifierMismatchErr: OSStatus { get }
var kVTMultiPassStorageInvalidErr: OSStatus { get }
var kVTFrameSiloInvalidTimeStampErr: OSStatus { get }
var kVTFrameSiloInvalidTimeRangeErr: OSStatus { get }
var kVTCouldNotFindTemporalFilterErr: OSStatus { get }
var kVTPixelTransferNotPermittedErr: OSStatus { get }
struct VTDecodeFrameFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var _EnableAsynchronousDecompression: VTDecodeFrameFlags { get }
  static var _DoNotOutputFrame: VTDecodeFrameFlags { get }
  static var _1xRealTimePlayback: VTDecodeFrameFlags { get }
  static var _EnableTemporalProcessing: VTDecodeFrameFlags { get }
}
struct VTDecodeInfoFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var asynchronous: VTDecodeInfoFlags { get }
  static var frameDropped: VTDecodeInfoFlags { get }
  static var imageBufferModifiable: VTDecodeInfoFlags { get }
}
struct VTEncodeInfoFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var asynchronous: VTEncodeInfoFlags { get }
  static var frameDropped: VTEncodeInfoFlags { get }
}
