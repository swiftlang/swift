
class DRFile : DRFSObject {
  init!(path path: String!)
}
extension DRFile {
  @discardableResult
  class func virtualFile(withName name: String!, data data: NSData!) -> DRFile!
  @discardableResult
  class func virtualFile(withName name: String!, dataProducer producer: AnyObject!) -> DRFile!
  init!(name name: String!, data data: NSData!)
  init!(name name: String!, dataProducer producer: AnyObject!)
}
extension DRFile {
  @discardableResult
  class func hardLinkPointing(to original: DRFile!, inFilesystem filesystem: String!) -> DRFile!
  @discardableResult
  class func symLinkPointing(to original: DRFSObject!, inFilesystem filesystem: String!) -> DRFile!
  @discardableResult
  class func finderAliasPointing(to original: DRFSObject!, inFilesystem filesystem: String!) -> DRFile!
  init!(linkType linkType: String!, pointingTo original: DRFSObject!, inFilesystem filesystem: String!)
}
@available(OSX 10.2, *)
let DRLinkTypeHardLink: String
@available(OSX 10.2, *)
let DRLinkTypeSymbolicLink: String
@available(OSX 10.2, *)
let DRLinkTypeFinderAlias: String
typealias DRFileFork = UInt32
var DRFileForkData: Int { get }
var DRFileForkResource: Int { get }
protocol DRFileDataProduction {
  @discardableResult
  func calculateSize(of file: DRFile!, fork fork: DRFileFork, estimating estimate: Bool) -> UInt64
  @discardableResult
  func prepareFile(forBurn file: DRFile!) -> Bool
  @discardableResult
  func produce(_ file: DRFile!, fork fork: DRFileFork, intoBuffer buffer: UnsafeMutablePointer<Int8>!, length bufferLength: UInt32, atAddress address: UInt64, blockSize blockSize: UInt32) -> UInt32
  @discardableResult
  func prepareFile(forVerification file: DRFile!) -> Bool
  func cleanupFile(afterBurn file: DRFile!)
}
