
var AUDIO_TOOLBOX_VERSION: Int32 { get }
@available(OSX 10.2, *)
func CAShow(_ inObject: UnsafeMutablePointer<Void>)
@available(OSX 10.2, *)
func CAShowFile(_ inObject: UnsafeMutablePointer<Void>, _ inFile: UnsafeMutablePointer<FILE>)
@available(OSX 10.5, *)
@discardableResult
func CopyNameFromSoundBank(_ inURL: CFURL, _ outName: UnsafeMutablePointer<Unmanaged<CFString>?>) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func CopyInstrumentInfoFromSoundBank(_ inURL: CFURL, _ outInstrumentInfo: UnsafeMutablePointer<Unmanaged<CFArray>?>) -> OSStatus
var kInstrumentInfoKey_Name: String { get }
var kInstrumentInfoKey_MSB: String { get }
var kInstrumentInfoKey_LSB: String { get }
var kInstrumentInfoKey_Program: String { get }
