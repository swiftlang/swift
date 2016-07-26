
var NSFoundationVersionWithFileManagerResourceForkSupport: Int32 { get }
@available(watchOS 2.0, *)
struct NSVolumeEnumerationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var skipHiddenVolumes: NSVolumeEnumerationOptions { get }
  static var produceFileReferenceURLs: NSVolumeEnumerationOptions { get }
}
@available(watchOS 2.0, *)
struct NSDirectoryEnumerationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var skipsSubdirectoryDescendants: NSDirectoryEnumerationOptions { get }
  static var skipsPackageDescendants: NSDirectoryEnumerationOptions { get }
  static var skipsHiddenFiles: NSDirectoryEnumerationOptions { get }
}
@available(watchOS 2.0, *)
struct NSFileManagerItemReplacementOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var usingNewMetadataOnly: NSFileManagerItemReplacementOptions { get }
  static var withoutDeletingBackupItem: NSFileManagerItemReplacementOptions { get }
}
@available(watchOS 2.0, *)
enum NSURLRelationship : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case contains
  case same
  case other
}
@available(watchOS 2.0, *)
let NSUbiquityIdentityDidChangeNotification: String
class NSFileManager : NSObject {
  @discardableResult
  class func defaultManager() -> NSFileManager
  @available(watchOS 2.0, *)
  @discardableResult
  func mountedVolumeURLs(includingResourceValuesForKeys propertyKeys: [String]?, options options: NSVolumeEnumerationOptions = []) -> [NSURL]?
  @available(watchOS 2.0, *)
  @discardableResult
  func contentsOfDirectory(at url: NSURL, includingPropertiesForKeys keys: [String]?, options mask: NSDirectoryEnumerationOptions = []) throws -> [NSURL]
  @available(watchOS 2.0, *)
  @discardableResult
  func urlsForDirectory(_ directory: NSSearchPathDirectory, inDomains domainMask: NSSearchPathDomainMask) -> [NSURL]
  @available(watchOS 2.0, *)
  @discardableResult
  func urlForDirectory(_ directory: NSSearchPathDirectory, in domain: NSSearchPathDomainMask, appropriateFor url: NSURL?, create shouldCreate: Bool) throws -> NSURL
  @available(watchOS 2.0, *)
  func getRelationship(_ outRelationship: UnsafeMutablePointer<NSURLRelationship>, ofDirectoryAt directoryURL: NSURL, toItemAt otherURL: NSURL) throws
  @available(watchOS 2.0, *)
  func getRelationship(_ outRelationship: UnsafeMutablePointer<NSURLRelationship>, of directory: NSSearchPathDirectory, in domainMask: NSSearchPathDomainMask, toItemAt url: NSURL) throws
  @available(watchOS 2.0, *)
  func createDirectory(at url: NSURL, withIntermediateDirectories createIntermediates: Bool, attributes attributes: [String : AnyObject]? = [:]) throws
  @available(watchOS 2.0, *)
  func createSymbolicLink(at url: NSURL, withDestinationURL destURL: NSURL) throws
  @available(watchOS 2.0, *)
  unowned(unsafe) var delegate: @sil_unmanaged NSFileManagerDelegate?
  @available(watchOS 2.0, *)
  func setAttributes(_ attributes: [String : AnyObject], ofItemAtPath path: String) throws
  @available(watchOS 2.0, *)
  func createDirectory(atPath path: String, withIntermediateDirectories createIntermediates: Bool, attributes attributes: [String : AnyObject]? = [:]) throws
  @available(watchOS 2.0, *)
  @discardableResult
  func contentsOfDirectory(atPath path: String) throws -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  func subpathsOfDirectory(atPath path: String) throws -> [String]
  @available(watchOS 2.0, *)
  @discardableResult
  func attributesOfItem(atPath path: String) throws -> [String : AnyObject]
  @available(watchOS 2.0, *)
  @discardableResult
  func attributesOfFileSystem(forPath path: String) throws -> [String : AnyObject]
  @available(watchOS 2.0, *)
  func createSymbolicLink(atPath path: String, withDestinationPath destPath: String) throws
  @available(watchOS 2.0, *)
  @discardableResult
  func destinationOfSymbolicLink(atPath path: String) throws -> String
  @available(watchOS 2.0, *)
  func copyItem(atPath srcPath: String, toPath dstPath: String) throws
  @available(watchOS 2.0, *)
  func moveItem(atPath srcPath: String, toPath dstPath: String) throws
  @available(watchOS 2.0, *)
  func linkItem(atPath srcPath: String, toPath dstPath: String) throws
  @available(watchOS 2.0, *)
  func removeItem(atPath path: String) throws
  @available(watchOS 2.0, *)
  func copyItem(at srcURL: NSURL, to dstURL: NSURL) throws
  @available(watchOS 2.0, *)
  func moveItem(at srcURL: NSURL, to dstURL: NSURL) throws
  @available(watchOS 2.0, *)
  func linkItem(at srcURL: NSURL, to dstURL: NSURL) throws
  @available(watchOS 2.0, *)
  func removeItem(at URL: NSURL) throws
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func fileAttributes(atPath path: String, traverseLink yorn: Bool) -> [NSObject : AnyObject]?
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func changeFileAttributes(_ attributes: [NSObject : AnyObject] = [:], atPath path: String) -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func directoryContents(atPath path: String) -> [AnyObject]?
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func fileSystemAttributes(atPath path: String) -> [NSObject : AnyObject]?
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func pathContentOfSymbolicLink(atPath path: String) -> String?
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func createSymbolicLink(atPath path: String, pathContent otherpath: String) -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func createDirectory(atPath path: String, attributes attributes: [NSObject : AnyObject] = [:]) -> Bool
  var currentDirectoryPath: String { get }
  @discardableResult
  func changeCurrentDirectoryPath(_ path: String) -> Bool
  @discardableResult
  func fileExists(atPath path: String) -> Bool
  @discardableResult
  func fileExists(atPath path: String, isDirectory isDirectory: UnsafeMutablePointer<ObjCBool>?) -> Bool
  @discardableResult
  func isReadableFile(atPath path: String) -> Bool
  @discardableResult
  func isWritableFile(atPath path: String) -> Bool
  @discardableResult
  func isExecutableFile(atPath path: String) -> Bool
  @discardableResult
  func isDeletableFile(atPath path: String) -> Bool
  @discardableResult
  func contentsEqual(atPath path1: String, andPath path2: String) -> Bool
  @discardableResult
  func displayName(atPath path: String) -> String
  @discardableResult
  func componentsToDisplay(forPath path: String) -> [String]?
  @discardableResult
  func enumerator(atPath path: String) -> NSDirectoryEnumerator?
  @available(watchOS 2.0, *)
  @discardableResult
  func enumerator(at url: NSURL, includingPropertiesForKeys keys: [String]?, options mask: NSDirectoryEnumerationOptions = [], errorHandler handler: ((NSURL, NSError) -> Bool)? = nil) -> NSDirectoryEnumerator?
  @discardableResult
  func subpaths(atPath path: String) -> [String]?
  @discardableResult
  func contents(atPath path: String) -> NSData?
  @discardableResult
  func createFile(atPath path: String, contents data: NSData?, attributes attr: [String : AnyObject]? = [:]) -> Bool
  @discardableResult
  func fileSystemRepresentation(withPath path: String) -> UnsafePointer<Int8>
  @discardableResult
  func string(withFileSystemRepresentation str: UnsafePointer<Int8>, length len: Int) -> String
  @available(watchOS 2.0, *)
  func replaceItem(at originalItemURL: NSURL, withItemAt newItemURL: NSURL, backupItemName backupItemName: String?, options options: NSFileManagerItemReplacementOptions = [], resultingItemURL resultingURL: AutoreleasingUnsafeMutablePointer<NSURL?>?) throws
  @available(watchOS 2.0, *)
  func setUbiquitous(_ flag: Bool, itemAt url: NSURL, destinationURL destinationURL: NSURL) throws
  @available(watchOS 2.0, *)
  @discardableResult
  func isUbiquitousItem(at url: NSURL) -> Bool
  @available(watchOS 2.0, *)
  func startDownloadingUbiquitousItem(at url: NSURL) throws
  @available(watchOS 2.0, *)
  func evictUbiquitousItem(at url: NSURL) throws
  @available(watchOS 2.0, *)
  @discardableResult
  func urlForUbiquityContainerIdentifier(_ containerIdentifier: String?) -> NSURL?
  @available(watchOS 2.0, *)
  @discardableResult
  func urlForPublishingUbiquitousItem(at url: NSURL, expirationDate outDate: AutoreleasingUnsafeMutablePointer<NSDate?>?) throws -> NSURL
  @available(watchOS 2.0, *)
  @NSCopying var ubiquityIdentityToken: protocol<NSCoding, NSCopying, NSObjectProtocol>? { get }
  @available(watchOS 2.0, *)
  @discardableResult
  func containerURLForSecurityApplicationGroupIdentifier(_ groupIdentifier: String) -> NSURL?
}
extension NSObject {
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  class func fileManager(_ fm: NSFileManager, shouldProceedAfterError errorInfo: [NSObject : AnyObject]) -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  @discardableResult
  func fileManager(_ fm: NSFileManager, shouldProceedAfterError errorInfo: [NSObject : AnyObject]) -> Bool
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  class func fileManager(_ fm: NSFileManager, willProcessPath path: String)
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  func fileManager(_ fm: NSFileManager, willProcessPath path: String)
}
protocol NSFileManagerDelegate : NSObjectProtocol {
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldCopyItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldCopyItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, copyingItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, copyingItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldMoveItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldMoveItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, movingItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, movingItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldLinkItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldLinkItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, linkingItemAtPath srcPath: String, toPath dstPath: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, linkingItemAt srcURL: NSURL, to dstURL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldRemoveItemAtPath path: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldRemoveItemAt URL: NSURL) -> Bool
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, removingItemAtPath path: String) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  optional func fileManager(_ fileManager: NSFileManager, shouldProceedAfterError error: NSError, removingItemAt URL: NSURL) -> Bool
}
class NSDirectoryEnumerator : NSEnumerator {
  var fileAttributes: [String : AnyObject]? { get }
  var directoryAttributes: [String : AnyObject]? { get }
  func skipDescendents()
  @available(watchOS 2.0, *)
  var level: Int { get }
  @available(watchOS 2.0, *)
  func skipDescendants()
}
let NSFileType: String
let NSFileTypeDirectory: String
let NSFileTypeRegular: String
let NSFileTypeSymbolicLink: String
let NSFileTypeSocket: String
let NSFileTypeCharacterSpecial: String
let NSFileTypeBlockSpecial: String
let NSFileTypeUnknown: String
let NSFileSize: String
let NSFileModificationDate: String
let NSFileReferenceCount: String
let NSFileDeviceIdentifier: String
let NSFileOwnerAccountName: String
let NSFileGroupOwnerAccountName: String
let NSFilePosixPermissions: String
let NSFileSystemNumber: String
let NSFileSystemFileNumber: String
let NSFileExtensionHidden: String
let NSFileHFSCreatorCode: String
let NSFileHFSTypeCode: String
let NSFileImmutable: String
let NSFileAppendOnly: String
let NSFileCreationDate: String
let NSFileOwnerAccountID: String
let NSFileGroupOwnerAccountID: String
let NSFileBusy: String
@available(watchOS 2.0, *)
let NSFileProtectionKey: String
@available(watchOS 2.0, *)
let NSFileProtectionNone: String
@available(watchOS 2.0, *)
let NSFileProtectionComplete: String
@available(watchOS 2.0, *)
let NSFileProtectionCompleteUnlessOpen: String
@available(watchOS 2.0, *)
let NSFileProtectionCompleteUntilFirstUserAuthentication: String
let NSFileSystemSize: String
let NSFileSystemFreeSize: String
let NSFileSystemNodes: String
let NSFileSystemFreeNodes: String
extension NSDictionary {
  @discardableResult
  func fileSize() -> UInt64
  @discardableResult
  func fileModificationDate() -> NSDate?
  @discardableResult
  func fileType() -> String?
  @discardableResult
  func filePosixPermissions() -> Int
  @discardableResult
  func fileOwnerAccountName() -> String?
  @discardableResult
  func fileGroupOwnerAccountName() -> String?
  @discardableResult
  func fileSystemNumber() -> Int
  @discardableResult
  func fileSystemFileNumber() -> Int
  @discardableResult
  func fileExtensionHidden() -> Bool
  @discardableResult
  func fileHFSCreatorCode() -> OSType
  @discardableResult
  func fileHFSTypeCode() -> OSType
  @discardableResult
  func fileIsImmutable() -> Bool
  @discardableResult
  func fileIsAppendOnly() -> Bool
  @discardableResult
  func fileCreationDate() -> NSDate?
  @discardableResult
  func fileOwnerAccountID() -> NSNumber?
  @discardableResult
  func fileGroupOwnerAccountID() -> NSNumber?
}
