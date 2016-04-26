
struct AudioFileStreamPropertyFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var propertyIsCached: AudioFileStreamPropertyFlags { get }
  static var cacheProperty: AudioFileStreamPropertyFlags { get }
}
struct AudioFileStreamParseFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var discontinuity: AudioFileStreamParseFlags { get }
}
struct AudioFileStreamSeekFlags : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var offsetIsEstimated: AudioFileStreamSeekFlags { get }
}
typealias AudioFileStreamPropertyID = UInt32
typealias AudioFileStreamID = OpaquePointer
typealias AudioFileStream_PropertyListenerProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileStreamID, AudioFileStreamPropertyID, UnsafeMutablePointer<AudioFileStreamPropertyFlags>) -> Void
typealias AudioFileStream_PacketsProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UnsafePointer<Void>, UnsafeMutablePointer<AudioStreamPacketDescription>) -> Void
var kAudioFileStreamError_UnsupportedFileType: OSStatus { get }
var kAudioFileStreamError_UnsupportedDataFormat: OSStatus { get }
var kAudioFileStreamError_UnsupportedProperty: OSStatus { get }
var kAudioFileStreamError_BadPropertySize: OSStatus { get }
var kAudioFileStreamError_NotOptimized: OSStatus { get }
var kAudioFileStreamError_InvalidPacketOffset: OSStatus { get }
var kAudioFileStreamError_InvalidFile: OSStatus { get }
var kAudioFileStreamError_ValueUnknown: OSStatus { get }
var kAudioFileStreamError_DataUnavailable: OSStatus { get }
var kAudioFileStreamError_IllegalOperation: OSStatus { get }
var kAudioFileStreamError_UnspecifiedError: OSStatus { get }
var kAudioFileStreamError_DiscontinuityCantRecover: OSStatus { get }
var kAudioFileStreamProperty_ReadyToProducePackets: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_FileFormat: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_DataFormat: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_FormatList: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_MagicCookieData: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_AudioDataByteCount: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_AudioDataPacketCount: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_MaximumPacketSize: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_DataOffset: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_ChannelLayout: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_PacketToFrame: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_FrameToPacket: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_PacketToByte: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_ByteToPacket: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_PacketTableInfo: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_PacketSizeUpperBound: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_AverageBytesPerPacket: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_BitRate: AudioFileStreamPropertyID { get }
var kAudioFileStreamProperty_InfoDictionary: AudioFileStreamPropertyID { get }
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamOpen(_ inClientData: UnsafeMutablePointer<Void>?, _ inPropertyListenerProc: AudioFileStream_PropertyListenerProc, _ inPacketsProc: AudioFileStream_PacketsProc, _ inFileTypeHint: AudioFileTypeID, _ outAudioFileStream: UnsafeMutablePointer<AudioFileStreamID?>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamParseBytes(_ inAudioFileStream: AudioFileStreamID, _ inDataByteSize: UInt32, _ inData: UnsafePointer<Void>, _ inFlags: AudioFileStreamParseFlags) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamSeek(_ inAudioFileStream: AudioFileStreamID, _ inPacketOffset: Int64, _ outDataByteOffset: UnsafeMutablePointer<Int64>, _ ioFlags: UnsafeMutablePointer<AudioFileStreamSeekFlags>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamGetPropertyInfo(_ inAudioFileStream: AudioFileStreamID, _ inPropertyID: AudioFileStreamPropertyID, _ outPropertyDataSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamGetProperty(_ inAudioFileStream: AudioFileStreamID, _ inPropertyID: AudioFileStreamPropertyID, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamSetProperty(_ inAudioFileStream: AudioFileStreamID, _ inPropertyID: AudioFileStreamPropertyID, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileStreamClose(_ inAudioFileStream: AudioFileStreamID) -> OSStatus
