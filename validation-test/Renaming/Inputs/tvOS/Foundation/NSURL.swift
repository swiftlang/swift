
class NSURL : NSObject, NSSecureCoding, NSCopying {
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLComponents instead, which lets you create a valid URL with any valid combination of URL components and subcomponents (not just scheme, host and path), and lets you set components and subcomponents with either percent-encoded or un-percent-encoded strings.")
  convenience init?(scheme scheme: String, host host: String?, path path: String)
  @available(tvOS 9.0, *)
  init(fileURLWithPath path: String, isDirectory isDir: Bool, relativeTo baseURL: NSURL?)
  @available(tvOS 9.0, *)
  init(fileURLWithPath path: String, relativeTo baseURL: NSURL?)
  @available(tvOS 2.0, *)
  init(fileURLWithPath path: String, isDirectory isDir: Bool)
  init(fileURLWithPath path: String)
  @available(tvOS 9.0, *)
  @discardableResult
  class func fileURL(withPath path: String, isDirectory isDir: Bool, relativeTo baseURL: NSURL?) -> NSURL
  @available(tvOS 9.0, *)
  @discardableResult
  class func fileURL(withPath path: String, relativeTo baseURL: NSURL?) -> NSURL
  @available(tvOS 2.0, *)
  @discardableResult
  class func fileURL(withPath path: String, isDirectory isDir: Bool) -> NSURL
  @discardableResult
  class func fileURL(withPath path: String) -> NSURL
  @available(tvOS 7.0, *)
  init(fileURLWithFileSystemRepresentation path: UnsafePointer<Int8>, isDirectory isDir: Bool, relativeTo baseURL: NSURL?)
  @available(tvOS 7.0, *)
  @discardableResult
  class func fileURL(withFileSystemRepresentation path: UnsafePointer<Int8>, isDirectory isDir: Bool, relativeTo baseURL: NSURL?) -> NSURL
  convenience init?(string URLString: String)
  init?(string URLString: String, relativeTo baseURL: NSURL?)
  @available(tvOS 9.0, *)
  init(dataRepresentation data: NSData, relativeTo baseURL: NSURL?)
  @available(tvOS 9.0, *)
  init(absoluteURLWithDataRepresentation data: NSData, relativeTo baseURL: NSURL?)
  @available(tvOS 9.0, *)
  @discardableResult
  class func absoluteURL(withDataRepresentation data: NSData, relativeTo baseURL: NSURL?) -> NSURL
  @available(tvOS 9.0, *)
  @NSCopying var dataRepresentation: NSData { get }
  var absoluteString: String { get }
  var relativeString: String? { get }
  @NSCopying var baseURL: NSURL? { get }
  @NSCopying var absoluteURL: NSURL { get }
  var scheme: String { get }
  var resourceSpecifier: String { get }
  var host: String? { get }
  @NSCopying var port: NSNumber? { get }
  var user: String? { get }
  var password: String? { get }
  var path: String? { get }
  var fragment: String? { get }
  var parameterString: String? { get }
  var query: String? { get }
  var relativePath: String? { get }
  @available(tvOS 9.0, *)
  var hasDirectoryPath: Bool { get }
  @available(tvOS 7.0, *)
  @discardableResult
  func getFileSystemRepresentation(_ buffer: UnsafeMutablePointer<Int8>, maxLength maxBufferLength: Int) -> Bool
  @available(tvOS 7.0, *)
  var fileSystemRepresentation: UnsafePointer<Int8> { get }
  var isFileURL: Bool { get }
  @NSCopying var standardized: NSURL? { get }
  @available(tvOS 4.0, *)
  @discardableResult
  func checkResourceIsReachableAndReturnError(_ error: NSErrorPointer) -> Bool
  @available(tvOS 4.0, *)
  @discardableResult
  func isFileReferenceURL() -> Bool
  @available(tvOS 4.0, *)
  @discardableResult
  func fileReferenceURL() -> NSURL?
  @available(tvOS 4.0, *)
  @NSCopying var filePathURL: NSURL? { get }
  @available(tvOS 4.0, *)
  func getResourceValue(_ value: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKey key: String) throws
  @available(tvOS 4.0, *)
  @discardableResult
  func resourceValues(forKeys keys: [String]) throws -> [String : AnyObject]
  @available(tvOS 4.0, *)
  func setResourceValue(_ value: AnyObject?, forKey key: String) throws
  @available(tvOS 4.0, *)
  func setResourceValues(_ keyedValues: [String : AnyObject]) throws
  @available(tvOS 7.0, *)
  func removeCachedResourceValue(forKey key: String)
  @available(tvOS 7.0, *)
  func removeAllCachedResourceValues()
  @available(tvOS 7.0, *)
  func setTemporaryResourceValue(_ value: AnyObject?, forKey key: String)
  @available(tvOS 4.0, *)
  @discardableResult
  func bookmarkData(_ options: NSURLBookmarkCreationOptions = [], includingResourceValuesForKeys keys: [String]?, relativeTo relativeURL: NSURL?) throws -> NSData
  @available(tvOS 4.0, *)
  convenience init(resolvingBookmarkData bookmarkData: NSData, options options: NSURLBookmarkResolutionOptions = [], relativeTo relativeURL: NSURL?, bookmarkDataIsStale isStale: UnsafeMutablePointer<ObjCBool>?) throws
  @available(tvOS 4.0, *)
  @discardableResult
  class func resourceValues(forKeys keys: [String], fromBookmarkData bookmarkData: NSData) -> [String : AnyObject]?
  @available(tvOS 4.0, *)
  class func writeBookmarkData(_ bookmarkData: NSData, to bookmarkFileURL: NSURL, options options: NSURLBookmarkFileCreationOptions) throws
  @available(tvOS 4.0, *)
  @discardableResult
  class func bookmarkData(withContentsOf bookmarkFileURL: NSURL) throws -> NSData
  @available(tvOS 8.0, *)
  convenience init(resolvingAliasFileAt url: NSURL, options options: NSURLBookmarkResolutionOptions = []) throws
  @available(tvOS 8.0, *)
  @discardableResult
  func startAccessingSecurityScopedResource() -> Bool
  @available(tvOS 8.0, *)
  func stopAccessingSecurityScopedResource()
}

extension NSURL : _FileReferenceLiteralConvertible {
  convenience init(failableFileReferenceLiteral path: String)
}

extension NSURL : CustomPlaygroundQuickLookable {
}
let NSURLFileScheme: String
@available(tvOS 5.0, *)
let NSURLKeysOfUnsetValuesKey: String
@available(tvOS 4.0, *)
let NSURLNameKey: String
@available(tvOS 4.0, *)
let NSURLLocalizedNameKey: String
@available(tvOS 4.0, *)
let NSURLIsRegularFileKey: String
@available(tvOS 4.0, *)
let NSURLIsDirectoryKey: String
@available(tvOS 4.0, *)
let NSURLIsSymbolicLinkKey: String
@available(tvOS 4.0, *)
let NSURLIsVolumeKey: String
@available(tvOS 4.0, *)
let NSURLIsPackageKey: String
@available(tvOS 9.0, *)
let NSURLIsApplicationKey: String
@available(tvOS 4.0, *)
let NSURLIsSystemImmutableKey: String
@available(tvOS 4.0, *)
let NSURLIsUserImmutableKey: String
@available(tvOS 4.0, *)
let NSURLIsHiddenKey: String
@available(tvOS 4.0, *)
let NSURLHasHiddenExtensionKey: String
@available(tvOS 4.0, *)
let NSURLCreationDateKey: String
@available(tvOS 4.0, *)
let NSURLContentAccessDateKey: String
@available(tvOS 4.0, *)
let NSURLContentModificationDateKey: String
@available(tvOS 4.0, *)
let NSURLAttributeModificationDateKey: String
@available(tvOS 4.0, *)
let NSURLLinkCountKey: String
@available(tvOS 4.0, *)
let NSURLParentDirectoryURLKey: String
@available(tvOS 4.0, *)
let NSURLVolumeURLKey: String
@available(tvOS 4.0, *)
let NSURLTypeIdentifierKey: String
@available(tvOS 4.0, *)
let NSURLLocalizedTypeDescriptionKey: String
@available(tvOS 4.0, *)
let NSURLLabelNumberKey: String
@available(tvOS 4.0, *)
let NSURLLabelColorKey: String
@available(tvOS 4.0, *)
let NSURLLocalizedLabelKey: String
@available(tvOS 4.0, *)
let NSURLEffectiveIconKey: String
@available(tvOS 4.0, *)
let NSURLCustomIconKey: String
@available(tvOS 5.0, *)
let NSURLFileResourceIdentifierKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIdentifierKey: String
@available(tvOS 5.0, *)
let NSURLPreferredIOBlockSizeKey: String
@available(tvOS 5.0, *)
let NSURLIsReadableKey: String
@available(tvOS 5.0, *)
let NSURLIsWritableKey: String
@available(tvOS 5.0, *)
let NSURLIsExecutableKey: String
@available(tvOS 5.0, *)
let NSURLFileSecurityKey: String
@available(tvOS 5.1, *)
let NSURLIsExcludedFromBackupKey: String
@available(tvOS 6.0, *)
let NSURLPathKey: String
@available(tvOS 5.0, *)
let NSURLIsMountTriggerKey: String
@available(tvOS 8.0, *)
let NSURLGenerationIdentifierKey: String
@available(tvOS 8.0, *)
let NSURLDocumentIdentifierKey: String
@available(tvOS 8.0, *)
let NSURLAddedToDirectoryDateKey: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeKey: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeNamedPipe: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeCharacterSpecial: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeDirectory: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeBlockSpecial: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeRegular: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeSymbolicLink: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeSocket: String
@available(tvOS 5.0, *)
let NSURLFileResourceTypeUnknown: String
@available(tvOS 8.0, *)
let NSURLThumbnailDictionaryKey: String
@available(tvOS 8.0, *)
let NSThumbnail1024x1024SizeKey: String
@available(tvOS 4.0, *)
let NSURLFileSizeKey: String
@available(tvOS 4.0, *)
let NSURLFileAllocatedSizeKey: String
@available(tvOS 5.0, *)
let NSURLTotalFileSizeKey: String
@available(tvOS 5.0, *)
let NSURLTotalFileAllocatedSizeKey: String
@available(tvOS 4.0, *)
let NSURLIsAliasFileKey: String
@available(tvOS 9.0, *)
let NSURLFileProtectionKey: String
@available(tvOS 9.0, *)
let NSURLFileProtectionNone: String
@available(tvOS 9.0, *)
let NSURLFileProtectionComplete: String
@available(tvOS 9.0, *)
let NSURLFileProtectionCompleteUnlessOpen: String
@available(tvOS 9.0, *)
let NSURLFileProtectionCompleteUntilFirstUserAuthentication: String
@available(tvOS 4.0, *)
let NSURLVolumeLocalizedFormatDescriptionKey: String
@available(tvOS 4.0, *)
let NSURLVolumeTotalCapacityKey: String
@available(tvOS 4.0, *)
let NSURLVolumeAvailableCapacityKey: String
@available(tvOS 4.0, *)
let NSURLVolumeResourceCountKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsPersistentIDsKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsSymbolicLinksKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsHardLinksKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsJournalingKey: String
@available(tvOS 4.0, *)
let NSURLVolumeIsJournalingKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsSparseFilesKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsZeroRunsKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsCaseSensitiveNamesKey: String
@available(tvOS 4.0, *)
let NSURLVolumeSupportsCasePreservedNamesKey: String
@available(tvOS 5.0, *)
let NSURLVolumeSupportsRootDirectoryDatesKey: String
@available(tvOS 5.0, *)
let NSURLVolumeSupportsVolumeSizesKey: String
@available(tvOS 5.0, *)
let NSURLVolumeSupportsRenamingKey: String
@available(tvOS 5.0, *)
let NSURLVolumeSupportsAdvisoryFileLockingKey: String
@available(tvOS 5.0, *)
let NSURLVolumeSupportsExtendedSecurityKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsBrowsableKey: String
@available(tvOS 5.0, *)
let NSURLVolumeMaximumFileSizeKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsEjectableKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsRemovableKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsInternalKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsAutomountedKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsLocalKey: String
@available(tvOS 5.0, *)
let NSURLVolumeIsReadOnlyKey: String
@available(tvOS 5.0, *)
let NSURLVolumeCreationDateKey: String
@available(tvOS 5.0, *)
let NSURLVolumeURLForRemountingKey: String
@available(tvOS 5.0, *)
let NSURLVolumeUUIDStringKey: String
@available(tvOS 5.0, *)
let NSURLVolumeNameKey: String
@available(tvOS 5.0, *)
let NSURLVolumeLocalizedNameKey: String
@available(tvOS 5.0, *)
let NSURLIsUbiquitousItemKey: String
@available(tvOS 5.0, *)
let NSURLUbiquitousItemHasUnresolvedConflictsKey: String
@available(tvOS 5.0, *)
let NSURLUbiquitousItemIsDownloadingKey: String
@available(tvOS 5.0, *)
let NSURLUbiquitousItemIsUploadedKey: String
@available(tvOS 5.0, *)
let NSURLUbiquitousItemIsUploadingKey: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemDownloadingStatusKey: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemDownloadingErrorKey: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemUploadingErrorKey: String
@available(tvOS 8.0, *)
let NSURLUbiquitousItemDownloadRequestedKey: String
@available(tvOS 8.0, *)
let NSURLUbiquitousItemContainerDisplayNameKey: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemDownloadingStatusNotDownloaded: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemDownloadingStatusDownloaded: String
@available(tvOS 7.0, *)
let NSURLUbiquitousItemDownloadingStatusCurrent: String
@available(tvOS 4.0, *)
struct NSURLBookmarkCreationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var minimalBookmark: NSURLBookmarkCreationOptions { get }
  static var suitableForBookmarkFile: NSURLBookmarkCreationOptions { get }
}
@available(tvOS 4.0, *)
struct NSURLBookmarkResolutionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var withoutUI: NSURLBookmarkResolutionOptions { get }
  static var withoutMounting: NSURLBookmarkResolutionOptions { get }
}
typealias NSURLBookmarkFileCreationOptions = Int
extension NSURL {
  @available(tvOS 8.0, *)
  func getPromisedItemResourceValue(_ value: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKey key: String) throws
  @available(tvOS 8.0, *)
  @discardableResult
  func promisedItemResourceValues(forKeys keys: [String]) throws -> [String : AnyObject]
  @available(tvOS 8.0, *)
  @discardableResult
  func checkPromisedItemIsReachableAndReturnError(_ error: NSErrorPointer) -> Bool
}
@available(tvOS 8.0, *)
class NSURLQueryItem : NSObject, NSSecureCoding, NSCopying {
  init(name name: String, value value: String?)
  var name: String { get }
  var value: String? { get }
}
@available(tvOS 7.0, *)
class NSURLComponents : NSObject, NSCopying {
  init?(url url: NSURL, resolvingAgainstBaseURL resolve: Bool)
  init?(string URLString: String)
  @NSCopying var url: NSURL? { get }
  @discardableResult
  func url(relativeTo baseURL: NSURL?) -> NSURL?
  @available(tvOS 8.0, *)
  var string: String? { get }
  var scheme: String?
  var user: String?
  var password: String?
  var host: String?
  @NSCopying var port: NSNumber?
  var path: String?
  var query: String?
  var fragment: String?
  var percentEncodedUser: String?
  var percentEncodedPassword: String?
  var percentEncodedHost: String?
  var percentEncodedPath: String?
  var percentEncodedQuery: String?
  var percentEncodedFragment: String?
  @available(tvOS 9.0, *)
  var rangeOfScheme: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfUser: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfPassword: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfHost: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfPort: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfPath: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfQuery: NSRange { get }
  @available(tvOS 9.0, *)
  var rangeOfFragment: NSRange { get }
  @available(tvOS 8.0, *)
  var queryItems: [NSURLQueryItem]?
}
extension NSCharacterSet {
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlUserAllowed() -> NSCharacterSet
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlPasswordAllowed() -> NSCharacterSet
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlHostAllowed() -> NSCharacterSet
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlPathAllowed() -> NSCharacterSet
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlQueryAllowed() -> NSCharacterSet
  @available(tvOS 7.0, *)
  @discardableResult
  class func urlFragmentAllowed() -> NSCharacterSet
}
extension NSString {
  @available(tvOS 7.0, *)
  @discardableResult
  func addingPercentEncoding(withAllowedCharacters allowedCharacters: NSCharacterSet) -> String?
  @available(tvOS 7.0, *)
  var removingPercentEncoding: String? { get }
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use -stringByAddingPercentEncodingWithAllowedCharacters: instead, which always uses the recommended UTF-8 encoding, and which encodes for a specific URL component or subcomponent since each URL component or subcomponent has different rules for what characters are valid.")
  @discardableResult
  func addingPercentEscapes(using enc: UInt) -> String?
  @available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use -stringByRemovingPercentEncoding instead, which always uses the recommended UTF-8 encoding.")
  @discardableResult
  func replacingPercentEscapes(using enc: UInt) -> String?
}
extension NSURL {
  @available(tvOS 4.0, *)
  @discardableResult
  class func fileURL(withPathComponents components: [String]) -> NSURL?
  @available(tvOS 4.0, *)
  var pathComponents: [String]? { get }
  @available(tvOS 4.0, *)
  var lastPathComponent: String? { get }
  @available(tvOS 4.0, *)
  var pathExtension: String? { get }
  @available(tvOS 4.0, *)
  @discardableResult
  func appendingPathComponent(_ pathComponent: String) -> NSURL
  @available(tvOS 5.0, *)
  @discardableResult
  func appendingPathComponent(_ pathComponent: String, isDirectory isDirectory: Bool) -> NSURL
  @available(tvOS 4.0, *)
  @NSCopying var deletingLastPathComponent: NSURL? { get }
  @available(tvOS 4.0, *)
  @discardableResult
  func appendingPathExtension(_ pathExtension: String) -> NSURL
  @available(tvOS 4.0, *)
  @NSCopying var deletingPathExtension: NSURL? { get }
  @available(tvOS 4.0, *)
  @NSCopying var standardizingPath: NSURL? { get }
  @available(tvOS 4.0, *)
  @NSCopying var resolvingSymlinksInPath: NSURL? { get }
}
@available(tvOS 5.0, *)
class NSFileSecurity : NSObject, NSCopying, NSCoding {
}
