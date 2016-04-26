
typealias AudioFileComponent = AudioComponentInstance
typealias AudioFileComponentPropertyID = UInt32
@available(OSX 10.5, *)
@discardableResult
func AudioFileComponentCreateURL(_ inComponent: AudioFileComponent, _ inFileRef: CFURL, _ inFormat: UnsafePointer<AudioStreamBasicDescription>, _ inFlags: UInt32) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileComponentOpenURL(_ inComponent: AudioFileComponent, _ inFileRef: CFURL, _ inPermissions: Int8, _ inFileDescriptor: Int32) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentOpenWithCallbacks(_ inComponent: AudioFileComponent, _ inClientData: UnsafeMutablePointer<Void>, _ inReadFunc: AudioFile_ReadProc, _ inWriteFunc: AudioFile_WriteProc, _ inGetSizeFunc: AudioFile_GetSizeProc, _ inSetSizeFunc: AudioFile_SetSizeProc) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentInitializeWithCallbacks(_ inComponent: AudioFileComponent, _ inClientData: UnsafeMutablePointer<Void>, _ inReadFunc: AudioFile_ReadProc, _ inWriteFunc: AudioFile_WriteProc, _ inGetSizeFunc: AudioFile_GetSizeProc, _ inSetSizeFunc: AudioFile_SetSizeProc, _ inFileType: UInt32, _ inFormat: UnsafePointer<AudioStreamBasicDescription>, _ inFlags: UInt32) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentCloseFile(_ inComponent: AudioFileComponent) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentOptimize(_ inComponent: AudioFileComponent) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentReadBytes(_ inComponent: AudioFileComponent, _ inUseCache: Bool, _ inStartingByte: Int64, _ ioNumBytes: UnsafeMutablePointer<UInt32>, _ outBuffer: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentWriteBytes(_ inComponent: AudioFileComponent, _ inUseCache: Bool, _ inStartingByte: Int64, _ ioNumBytes: UnsafeMutablePointer<UInt32>, _ inBuffer: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentReadPackets(_ inComponent: AudioFileComponent, _ inUseCache: Bool, _ outNumBytes: UnsafeMutablePointer<UInt32>, _ outPacketDescriptions: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ inStartingPacket: Int64, _ ioNumPackets: UnsafeMutablePointer<UInt32>, _ outBuffer: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentReadPacketData(_ inComponent: AudioFileComponent, _ inUseCache: Bool, _ ioNumBytes: UnsafeMutablePointer<UInt32>, _ outPacketDescriptions: UnsafeMutablePointer<AudioStreamPacketDescription>?, _ inStartingPacket: Int64, _ ioNumPackets: UnsafeMutablePointer<UInt32>, _ outBuffer: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentWritePackets(_ inComponent: AudioFileComponent, _ inUseCache: Bool, _ inNumBytes: UInt32, _ inPacketDescriptions: UnsafePointer<AudioStreamPacketDescription>?, _ inStartingPacket: Int64, _ ioNumPackets: UnsafeMutablePointer<UInt32>, _ inBuffer: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetPropertyInfo(_ inComponent: AudioFileComponent, _ inPropertyID: AudioFileComponentPropertyID, _ outPropertySize: UnsafeMutablePointer<UInt32>?, _ outWritable: UnsafeMutablePointer<UInt32>?) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetProperty(_ inComponent: AudioFileComponent, _ inPropertyID: AudioFileComponentPropertyID, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentSetProperty(_ inComponent: AudioFileComponent, _ inPropertyID: AudioFileComponentPropertyID, _ inPropertyDataSize: UInt32, _ inPropertyData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentCountUserData(_ inComponent: AudioFileComponent, _ inUserDataID: UInt32, _ outNumberItems: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetUserDataSize(_ inComponent: AudioFileComponent, _ inUserDataID: UInt32, _ inIndex: UInt32, _ outUserDataSize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetUserData(_ inComponent: AudioFileComponent, _ inUserDataID: UInt32, _ inIndex: UInt32, _ ioUserDataSize: UnsafeMutablePointer<UInt32>, _ outUserData: UnsafeMutablePointer<Void>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentSetUserData(_ inComponent: AudioFileComponent, _ inUserDataID: UInt32, _ inIndex: UInt32, _ inUserDataSize: UInt32, _ inUserData: UnsafePointer<Void>) -> OSStatus
@available(OSX 10.5, *)
@discardableResult
func AudioFileComponentRemoveUserData(_ inComponent: AudioFileComponent, _ inUserDataID: UInt32, _ inIndex: UInt32) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentExtensionIsThisFormat(_ inComponent: AudioFileComponent, _ inExtension: CFString, _ outResult: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentFileDataIsThisFormat(_ inComponent: AudioFileComponent, _ inDataByteSize: UInt32, _ inData: UnsafePointer<Void>, _ outResult: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetGlobalInfoSize(_ inComponent: AudioFileComponent, _ inPropertyID: AudioFileComponentPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ outPropertySize: UnsafeMutablePointer<UInt32>) -> OSStatus
@available(OSX 10.4, *)
@discardableResult
func AudioFileComponentGetGlobalInfo(_ inComponent: AudioFileComponent, _ inPropertyID: AudioFileComponentPropertyID, _ inSpecifierSize: UInt32, _ inSpecifier: UnsafePointer<Void>?, _ ioPropertyDataSize: UnsafeMutablePointer<UInt32>, _ outPropertyData: UnsafeMutablePointer<Void>) -> OSStatus
var kAudioFileComponent_CanRead: AudioFilePropertyID { get }
var kAudioFileComponent_CanWrite: AudioFilePropertyID { get }
var kAudioFileComponent_FileTypeName: AudioFilePropertyID { get }
var kAudioFileComponent_UTIsForType: AudioFilePropertyID { get }
var kAudioFileComponent_MIMETypesForType: AudioFilePropertyID { get }
var kAudioFileComponent_ExtensionsForType: AudioFilePropertyID { get }
var kAudioFileComponent_AvailableFormatIDs: AudioFilePropertyID { get }
var kAudioFileComponent_AvailableStreamDescriptionsForFormat: AudioFilePropertyID { get }
var kAudioFileComponent_FastDispatchTable: AudioFilePropertyID { get }
var kAudioFileComponent_HFSTypeCodesForType: AudioFilePropertyID { get }
var kAudioFileCreateSelect: Int { get }
var kAudioFileOpenSelect: Int { get }
var kAudioFileInitializeSelect: Int { get }
var kAudioFileOpenWithCallbacksSelect: Int { get }
var kAudioFileInitializeWithCallbacksSelect: Int { get }
var kAudioFileCloseSelect: Int { get }
var kAudioFileOptimizeSelect: Int { get }
var kAudioFileReadBytesSelect: Int { get }
var kAudioFileWriteBytesSelect: Int { get }
var kAudioFileReadPacketsSelect: Int { get }
var kAudioFileWritePacketsSelect: Int { get }
var kAudioFileGetPropertyInfoSelect: Int { get }
var kAudioFileGetPropertySelect: Int { get }
var kAudioFileSetPropertySelect: Int { get }
var kAudioFileExtensionIsThisFormatSelect: Int { get }
var kAudioFileFileIsThisFormatSelect: Int { get }
var kAudioFileDataIsThisFormatSelect: Int { get }
var kAudioFileGetGlobalInfoSizeSelect: Int { get }
var kAudioFileGetGlobalInfoSelect: Int { get }
var kAudioFileCountUserDataSelect: Int { get }
var kAudioFileGetUserDataSizeSelect: Int { get }
var kAudioFileGetUserDataSelect: Int { get }
var kAudioFileSetUserDataSelect: Int { get }
var kAudioFileRemoveUserDataSelect: Int { get }
var kAudioFileCreateURLSelect: Int { get }
var kAudioFileOpenURLSelect: Int { get }
var kAudioFileFileDataIsThisFormatSelect: Int { get }
var kAudioFileReadPacketDataSelect: Int { get }
typealias ReadBytesFDF = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias WriteBytesFDF = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, Int64, UnsafeMutablePointer<UInt32>, UnsafePointer<Void>) -> OSStatus
typealias ReadPacketsFDF = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias ReadPacketDataFDF = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias WritePacketsFDF = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UInt32, UnsafePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafePointer<Void>) -> OSStatus
typealias GetPropertyInfoFDF = @convention(c) (UnsafeMutablePointer<Void>, AudioFilePropertyID, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<UInt32>?) -> OSStatus
typealias GetPropertyFDF = @convention(c) (UnsafeMutablePointer<Void>, AudioFilePropertyID, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias SetPropertyFDF = @convention(c) (UnsafeMutablePointer<Void>, AudioFilePropertyID, UInt32, UnsafePointer<Void>) -> OSStatus
typealias CountUserDataFDF = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias GetUserDataSizeFDF = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias GetUserDataFDF = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias SetUserDataFDF = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UInt32, UnsafePointer<Void>) -> OSStatus
typealias AudioFileComponentCreateURLProc = @convention(c) (UnsafeMutablePointer<Void>, CFURL, UnsafePointer<AudioStreamBasicDescription>, UInt32) -> OSStatus
typealias AudioFileComponentOpenURLProc = @convention(c) (UnsafeMutablePointer<Void>, CFURL, Int8, Int32) -> OSStatus
typealias AudioFileComponentOpenWithCallbacksProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<Void>, AudioFile_ReadProc, AudioFile_WriteProc, AudioFile_GetSizeProc, AudioFile_SetSizeProc) -> OSStatus
typealias AudioFileComponentInitializeWithCallbacksProc = @convention(c) (UnsafeMutablePointer<Void>, UnsafeMutablePointer<Void>, AudioFile_ReadProc, AudioFile_WriteProc, AudioFile_GetSizeProc, AudioFile_SetSizeProc, UInt32, UnsafePointer<AudioStreamBasicDescription>, UInt32) -> OSStatus
typealias AudioFileComponentCloseProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentOptimizeProc = @convention(c) (UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentReadBytesProc = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentWriteBytesProc = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, Int64, UnsafeMutablePointer<UInt32>, UnsafePointer<Void>) -> OSStatus
typealias AudioFileComponentReadPacketsProc = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentReadPacketDataProc = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentWritePacketsProc = @convention(c) (UnsafeMutablePointer<Void>, DarwinBoolean, UInt32, UnsafePointer<AudioStreamPacketDescription>?, Int64, UnsafeMutablePointer<UInt32>, UnsafePointer<Void>) -> OSStatus
typealias AudioFileComponentGetPropertyInfoProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileComponentPropertyID, UnsafeMutablePointer<UInt32>?, UnsafeMutablePointer<UInt32>?) -> OSStatus
typealias AudioFileComponentGetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileComponentPropertyID, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentSetPropertyProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileComponentPropertyID, UInt32, UnsafePointer<Void>) -> OSStatus
typealias AudioFileComponentCountUserDataProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioFileComponentGetUserDataSizeProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioFileComponentGetUserDataProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
typealias AudioFileComponentSetUserDataProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32, UInt32, UnsafePointer<Void>) -> OSStatus
typealias AudioFileComponentRemoveUserDataProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UInt32) -> OSStatus
typealias AudioFileComponentExtensionIsThisFormatProc = @convention(c) (UnsafeMutablePointer<Void>, CFString, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioFileComponentFileDataIsThisFormatProc = @convention(c) (UnsafeMutablePointer<Void>, UInt32, UnsafePointer<Void>, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioFileComponentGetGlobalInfoSizeProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileComponentPropertyID, UInt32, UnsafePointer<Void>?, UnsafeMutablePointer<UInt32>) -> OSStatus
typealias AudioFileComponentGetGlobalInfoProc = @convention(c) (UnsafeMutablePointer<Void>, AudioFileComponentPropertyID, UInt32, UnsafePointer<Void>?, UnsafeMutablePointer<UInt32>, UnsafeMutablePointer<Void>) -> OSStatus
