
typealias ExtAudioFileRef = OpaquePointer
typealias ExtAudioFilePropertyID = UInt32
var kExtAudioFileProperty_FileDataFormat: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_FileChannelLayout: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_ClientDataFormat: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_ClientChannelLayout: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_CodecManufacturer: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_AudioConverter: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_AudioFile: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_FileMaxPacketSize: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_ClientMaxPacketSize: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_FileLengthFrames: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_ConverterConfig: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_IOBufferSizeBytes: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_IOBuffer: ExtAudioFilePropertyID { get }
var kExtAudioFileProperty_PacketTable: ExtAudioFilePropertyID { get }
var kExtAudioFileError_InvalidProperty: OSStatus { get }
var kExtAudioFileError_InvalidPropertySize: OSStatus { get }
var kExtAudioFileError_NonPCMClientFormat: OSStatus { get }
var kExtAudioFileError_InvalidChannelMap: OSStatus { get }
var kExtAudioFileError_InvalidOperationOrder: OSStatus { get }
var kExtAudioFileError_InvalidDataFormat: OSStatus { get }
var kExtAudioFileError_MaxPacketSizeUnknown: OSStatus { get }
var kExtAudioFileError_InvalidSeek: OSStatus { get }
var kExtAudioFileError_AsyncWriteTooLarge: OSStatus { get }
var kExtAudioFileError_AsyncWriteBufferOverflow: OSStatus { get }
var kExtAudioFileError_CodecUnavailableInputConsumed: OSStatus { get }
var kExtAudioFileError_CodecUnavailableInputNotConsumed: OSStatus { get }
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileOpenURL(_ inURL: CFURL, _ outExtAudioFile: UnsafeMutablePointer<ExtAudioFileRef?>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileWrapAudioFileID(_ inFileID: AudioFileID, _ inForWriting: Bool, _ outExtAudioFile: UnsafeMutablePointer<ExtAudioFileRef?>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileCreateWithURL(_ inURL: CFURL, _ inFileType: AudioFileTypeID, _ inStreamDesc: UnsafePointer<AudioStreamBasicDescription>, _ inChannelLayout: UnsafePointer<AudioChannelLayout>?, _ inFlags: UInt32, _ outExtAudioFile: UnsafeMutablePointer<ExtAudioFileRef?>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileDispose(_ inExtAudioFile: ExtAudioFileRef) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileRead(_ inExtAudioFile: ExtAudioFileRef, _ ioNumberFrames: UnsafeMutablePointer<UInt32>, _ ioData: UnsafeMutablePointer<AudioBufferList>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileWrite(_ inExtAudioFile: ExtAudioFileRef, _ inNumberFrames: UInt32, _ ioData: UnsafePointer<AudioBufferList>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileWriteAsync(_ inExtAudioFile: ExtAudioFileRef, _ inNumberFrames: UInt32, _ ioData: UnsafePointer<AudioBufferList>?) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileSeek(_ inExtAudioFile: ExtAudioFileRef, _ inFrameOffset: Int64) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileTell(_ inExtAudioFile: ExtAudioFileRef, _ outFrameOffset: UnsafeMutablePointer<Int64>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileGetPropertyInfo(_ inExtAudioFile: ExtAudioFileRef, _ inPropertyID: ExtAudioFilePropertyID, _ outSize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<DarwinBoolean>?) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileGetProperty(_ inExtAudioFile: ExtAudioFileRef, _ inPropertyID: ExtAudioFilePropertyID, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
@available(iOS 2.1, *)
@discardableResult
func ExtAudioFileSetProperty(_ inExtAudioFile: ExtAudioFileRef, _ inPropertyID: ExtAudioFilePropertyID, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
