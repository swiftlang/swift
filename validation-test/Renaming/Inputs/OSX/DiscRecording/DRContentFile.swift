
@available(OSX 10.2, *)
@discardableResult
func DRFileGetTypeID() -> CFTypeID
typealias DRLinkType = UInt32
var kDRLinkTypeHardLink: Int { get }
var kDRLinkTypeSymbolicLink: Int { get }
var kDRLinkTypeFinderAlias: Int { get }
typealias DRFileMessage = UInt32
var kDRFileMessageForkSize: Int { get }
var kDRFileMessagePreBurn: Int { get }
var kDRFileMessageProduceData: Int { get }
var kDRFileMessageVerificationStarting: Int { get }
var kDRFileMessagePostBurn: Int { get }
var kDRFileMessageRelease: Int { get }
typealias DRFileForkIndex = UInt32
var kDRFileForkData: Int { get }
var kDRFileForkResource: Int { get }
typealias DRFileForkSizeQuery = UInt32
var kDRFileForkSizeActual: Int { get }
var kDRFileForkSizeEstimate: Int { get }
struct DRFileForkSizeInfo {
  var fork: DRFileForkIndex
  var query: DRFileForkSizeQuery
  var size: UInt64
  init()
  init(fork fork: DRFileForkIndex, query query: DRFileForkSizeQuery, size size: UInt64)
}
struct DRFileProductionInfo {
  var requestedAddress: UInt64
  var buffer: UnsafeMutablePointer<Void>!
  var reqCount: UInt32
  var actCount: UInt32
  var blockSize: UInt32
  var fork: DRFileForkIndex
  init()
  init(requestedAddress requestedAddress: UInt64, buffer buffer: UnsafeMutablePointer<Void>!, reqCount reqCount: UInt32, actCount actCount: UInt32, blockSize blockSize: UInt32, fork fork: DRFileForkIndex)
}
typealias DRFileProc = @convention(c) (UnsafeMutablePointer<Void>!, DRFileRef!, DRFileMessage, UnsafeMutablePointer<Void>!) -> OSStatus
@available(OSX 10.2, *)
@discardableResult
func DRFileCreateReal(_ fsRef: UnsafePointer<FSRef>!) -> Unmanaged<DRFileRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFileCreateRealWithURL(_ urlRef: CFURL!) -> Unmanaged<DRFileRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFileCreateVirtualWithData(_ baseName: CFString!, _ fileData: UnsafeMutablePointer<Void>!, _ fileDataLength: UInt32) -> Unmanaged<DRFileRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFileCreateVirtualWithCallback(_ baseName: CFString!, _ fileProc: DRFileProc!, _ fileProcRefCon: UnsafeMutablePointer<Void>!) -> Unmanaged<DRFileRef>!
@available(OSX 10.2, *)
@discardableResult
func DRFileCreateVirtualLink(_ original: DRFSObjectRef!, _ linkType: DRLinkType, _ fsKey: CFString!) -> Unmanaged<DRFileRef>!
