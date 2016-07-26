
enum CFURLPathStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case cfurlposixPathStyle
  case cfurlWindowsPathStyle
}
class CFURL {
}
@discardableResult
func CFURLGetTypeID() -> CFTypeID
@discardableResult
func CFURLCreateWithBytes(_ allocator: CFAllocator!, _ URLBytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ encoding: CFStringEncoding, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateData(_ allocator: CFAllocator!, _ url: CFURL!, _ encoding: CFStringEncoding, _ escapeWhitespace: Bool) -> CFData!
@discardableResult
func CFURLCreateWithString(_ allocator: CFAllocator!, _ URLString: CFString!, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateAbsoluteURLWithBytes(_ alloc: CFAllocator!, _ relativeURLBytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ encoding: CFStringEncoding, _ baseURL: CFURL!, _ useCompatibilityMode: Bool) -> CFURL!
@discardableResult
func CFURLCreateWithFileSystemPath(_ allocator: CFAllocator!, _ filePath: CFString!, _ pathStyle: CFURLPathStyle, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateFromFileSystemRepresentation(_ allocator: CFAllocator!, _ buffer: UnsafePointer<UInt8>!, _ bufLen: CFIndex, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateWithFileSystemPathRelativeToBase(_ allocator: CFAllocator!, _ filePath: CFString!, _ pathStyle: CFURLPathStyle, _ isDirectory: Bool, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateFromFileSystemRepresentationRelativeToBase(_ allocator: CFAllocator!, _ buffer: UnsafePointer<UInt8>!, _ bufLen: CFIndex, _ isDirectory: Bool, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLGetFileSystemRepresentation(_ url: CFURL!, _ resolveAgainstBase: Bool, _ buffer: UnsafeMutablePointer<UInt8>!, _ maxBufLen: CFIndex) -> Bool
@discardableResult
func CFURLCopyAbsoluteURL(_ relativeURL: CFURL!) -> CFURL!
@discardableResult
func CFURLGetString(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLGetBaseURL(_ anURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCanBeDecomposed(_ anURL: CFURL!) -> Bool
@discardableResult
func CFURLCopyScheme(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyNetLocation(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPath(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyStrictPath(_ anURL: CFURL!, _ isAbsolute: UnsafeMutablePointer<DarwinBoolean>!) -> CFString!
@discardableResult
func CFURLCopyFileSystemPath(_ anURL: CFURL!, _ pathStyle: CFURLPathStyle) -> CFString!
@discardableResult
func CFURLHasDirectoryPath(_ anURL: CFURL!) -> Bool
@discardableResult
func CFURLCopyResourceSpecifier(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyHostName(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLGetPortNumber(_ anURL: CFURL!) -> Int32
@discardableResult
func CFURLCopyUserName(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPassword(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyParameterString(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyQueryString(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyFragment(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyLastPathComponent(_ url: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPathExtension(_ url: CFURL!) -> CFString!
@discardableResult
func CFURLCreateCopyAppendingPathComponent(_ allocator: CFAllocator!, _ url: CFURL!, _ pathComponent: CFString!, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateCopyDeletingLastPathComponent(_ allocator: CFAllocator!, _ url: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateCopyAppendingPathExtension(_ allocator: CFAllocator!, _ url: CFURL!, _ extension: CFString!) -> CFURL!
@discardableResult
func CFURLCreateCopyDeletingPathExtension(_ allocator: CFAllocator!, _ url: CFURL!) -> CFURL!
@discardableResult
func CFURLGetBytes(_ url: CFURL!, _ buffer: UnsafeMutablePointer<UInt8>!, _ bufferLength: CFIndex) -> CFIndex
enum CFURLComponentType : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case scheme
  case netLocation
  case path
  case resourceSpecifier
  case user
  case password
  case userInfo
  case host
  case port
  case parameterString
  case query
  case fragment
}
@discardableResult
func CFURLGetByteRangeForComponent(_ url: CFURL!, _ component: CFURLComponentType, _ rangeIncludingSeparators: UnsafeMutablePointer<CFRange>!) -> CFRange
@discardableResult
func CFURLCreateStringByReplacingPercentEscapes(_ allocator: CFAllocator!, _ originalString: CFString!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use [NSString stringByRemovingPercentEncoding] or CFURLCreateStringByReplacingPercentEscapes() instead, which always uses the recommended UTF-8 encoding.")
@discardableResult
func CFURLCreateStringByReplacingPercentEscapesUsingEncoding(_ allocator: CFAllocator!, _ origString: CFString!, _ charsToLeaveEscaped: CFString!, _ encoding: CFStringEncoding) -> CFString!
@available(OSX, introduced: 10.0, deprecated: 10.11, message: "Use [NSString stringByAddingPercentEncodingWithAllowedCharacters:] instead, which always uses the recommended UTF-8 encoding, and which encodes for a specific URL component or subcomponent (since each URL component or subcomponent has different rules for what characters are valid).")
@discardableResult
func CFURLCreateStringByAddingPercentEscapes(_ allocator: CFAllocator!, _ originalString: CFString!, _ charactersToLeaveUnescaped: CFString!, _ legalURLCharactersToBeEscaped: CFString!, _ encoding: CFStringEncoding) -> CFString!
@available(OSX 10.9, *)
@discardableResult
func CFURLIsFileReferenceURL(_ url: CFURL!) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateFileReferenceURL(_ allocator: CFAllocator!, _ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateFilePathURL(_ allocator: CFAllocator!, _ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCopyResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValueTypeRefPtr: UnsafeMutablePointer<Void>!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CFURLCopyResourcePropertiesForKeys(_ url: CFURL!, _ keys: CFArray!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFDictionary>!
@available(OSX 10.6, *)
@discardableResult
func CFURLSetResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValue: CFTypeRef!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CFURLSetResourcePropertiesForKeys(_ url: CFURL!, _ keyedPropertyValues: CFDictionary!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.7, *)
let kCFURLKeysOfUnsetValuesKey: CFString!
@available(OSX 10.6, *)
func CFURLClearResourcePropertyCacheForKey(_ url: CFURL!, _ key: CFString!)
@available(OSX 10.6, *)
func CFURLClearResourcePropertyCache(_ url: CFURL!)
@available(OSX 10.6, *)
func CFURLSetTemporaryResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValue: CFTypeRef!)
@available(OSX 10.6, *)
@discardableResult
func CFURLResourceIsReachable(_ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.6, *)
let kCFURLNameKey: CFString!
@available(OSX 10.6, *)
let kCFURLLocalizedNameKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsRegularFileKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsDirectoryKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsSymbolicLinkKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsVolumeKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsPackageKey: CFString!
@available(OSX 10.11, *)
let kCFURLIsApplicationKey: CFString!
@available(OSX 10.11, *)
let kCFURLApplicationIsScriptableKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsSystemImmutableKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsUserImmutableKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsHiddenKey: CFString!
@available(OSX 10.6, *)
let kCFURLHasHiddenExtensionKey: CFString!
@available(OSX 10.6, *)
let kCFURLCreationDateKey: CFString!
@available(OSX 10.6, *)
let kCFURLContentAccessDateKey: CFString!
@available(OSX 10.6, *)
let kCFURLContentModificationDateKey: CFString!
@available(OSX 10.6, *)
let kCFURLAttributeModificationDateKey: CFString!
@available(OSX 10.6, *)
let kCFURLLinkCountKey: CFString!
@available(OSX 10.6, *)
let kCFURLParentDirectoryURLKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeURLKey: CFString!
@available(OSX 10.6, *)
let kCFURLTypeIdentifierKey: CFString!
@available(OSX 10.6, *)
let kCFURLLocalizedTypeDescriptionKey: CFString!
@available(OSX 10.6, *)
let kCFURLLabelNumberKey: CFString!
@available(OSX 10.6, *)
let kCFURLLabelColorKey: CFString!
@available(OSX 10.6, *)
let kCFURLLocalizedLabelKey: CFString!
@available(OSX 10.6, *)
let kCFURLEffectiveIconKey: CFString!
@available(OSX 10.6, *)
let kCFURLCustomIconKey: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceIdentifierKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIdentifierKey: CFString!
@available(OSX 10.7, *)
let kCFURLPreferredIOBlockSizeKey: CFString!
@available(OSX 10.7, *)
let kCFURLIsReadableKey: CFString!
@available(OSX 10.7, *)
let kCFURLIsWritableKey: CFString!
@available(OSX 10.7, *)
let kCFURLIsExecutableKey: CFString!
@available(OSX 10.7, *)
let kCFURLFileSecurityKey: CFString!
@available(OSX 10.8, *)
let kCFURLIsExcludedFromBackupKey: CFString!
@available(OSX 10.9, *)
let kCFURLTagNamesKey: CFString!
@available(OSX 10.8, *)
let kCFURLPathKey: CFString!
@available(OSX 10.7, *)
let kCFURLIsMountTriggerKey: CFString!
@available(OSX 10.10, *)
let kCFURLGenerationIdentifierKey: CFString!
@available(OSX 10.10, *)
let kCFURLDocumentIdentifierKey: CFString!
@available(OSX 10.10, *)
let kCFURLAddedToDirectoryDateKey: CFString!
@available(OSX 10.10, *)
let kCFURLQuarantinePropertiesKey: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeKey: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeNamedPipe: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeCharacterSpecial: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeDirectory: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeBlockSpecial: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeRegular: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeSymbolicLink: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeSocket: CFString!
@available(OSX 10.7, *)
let kCFURLFileResourceTypeUnknown: CFString!
@available(OSX 10.6, *)
let kCFURLFileSizeKey: CFString!
@available(OSX 10.6, *)
let kCFURLFileAllocatedSizeKey: CFString!
@available(OSX 10.7, *)
let kCFURLTotalFileSizeKey: CFString!
@available(OSX 10.7, *)
let kCFURLTotalFileAllocatedSizeKey: CFString!
@available(OSX 10.6, *)
let kCFURLIsAliasFileKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeLocalizedFormatDescriptionKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeTotalCapacityKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeAvailableCapacityKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeResourceCountKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsPersistentIDsKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsSymbolicLinksKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsHardLinksKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsJournalingKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeIsJournalingKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsSparseFilesKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsZeroRunsKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsCaseSensitiveNamesKey: CFString!
@available(OSX 10.6, *)
let kCFURLVolumeSupportsCasePreservedNamesKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeSupportsRootDirectoryDatesKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeSupportsVolumeSizesKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeSupportsRenamingKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeSupportsAdvisoryFileLockingKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeSupportsExtendedSecurityKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsBrowsableKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeMaximumFileSizeKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsEjectableKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsRemovableKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsInternalKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsAutomountedKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsLocalKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeIsReadOnlyKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeCreationDateKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeURLForRemountingKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeUUIDStringKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeNameKey: CFString!
@available(OSX 10.7, *)
let kCFURLVolumeLocalizedNameKey: CFString!
@available(OSX 10.7, *)
let kCFURLIsUbiquitousItemKey: CFString!
@available(OSX 10.7, *)
let kCFURLUbiquitousItemHasUnresolvedConflictsKey: CFString!
@available(OSX 10.7, *)
let kCFURLUbiquitousItemIsDownloadingKey: CFString!
@available(OSX 10.7, *)
let kCFURLUbiquitousItemIsUploadedKey: CFString!
@available(OSX 10.7, *)
let kCFURLUbiquitousItemIsUploadingKey: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemDownloadingStatusKey: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemDownloadingErrorKey: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemUploadingErrorKey: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemDownloadingStatusNotDownloaded: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemDownloadingStatusDownloaded: CFString!
@available(OSX 10.9, *)
let kCFURLUbiquitousItemDownloadingStatusCurrent: CFString!
@available(OSX 10.6, *)
struct CFURLBookmarkCreationOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var minimalBookmarkMask: CFURLBookmarkCreationOptions { get }
  static var suitableForBookmarkFile: CFURLBookmarkCreationOptions { get }
  @available(OSX 10.7, *)
  static var withSecurityScope: CFURLBookmarkCreationOptions { get }
  @available(OSX 10.7, *)
  static var securityScopeAllowOnlyReadAccess: CFURLBookmarkCreationOptions { get }
}
@available(OSX 10.6, *)
struct CFURLBookmarkResolutionOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var cfurlBookmarkResolutionWithoutUIMask: CFURLBookmarkResolutionOptions { get }
  static var cfurlBookmarkResolutionWithoutMountingMask: CFURLBookmarkResolutionOptions { get }
  @available(OSX 10.7, *)
  static var cfurlBookmarkResolutionWithSecurityScope: CFURLBookmarkResolutionOptions { get }
  static var cfBookmarkResolutionWithoutUIMask: CFURLBookmarkResolutionOptions { get }
  static var cfBookmarkResolutionWithoutMountingMask: CFURLBookmarkResolutionOptions { get }
}
typealias CFURLBookmarkFileCreationOptions = CFOptionFlags
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateBookmarkData(_ allocator: CFAllocator!, _ url: CFURL!, _ options: CFURLBookmarkCreationOptions, _ resourcePropertiesToInclude: CFArray!, _ relativeToURL: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFData>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateByResolvingBookmarkData(_ allocator: CFAllocator!, _ bookmark: CFData!, _ options: CFURLBookmarkResolutionOptions, _ relativeToURL: CFURL!, _ resourcePropertiesToInclude: CFArray!, _ isStale: UnsafeMutablePointer<DarwinBoolean>!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateResourcePropertiesForKeysFromBookmarkData(_ allocator: CFAllocator!, _ resourcePropertiesToReturn: CFArray!, _ bookmark: CFData!) -> Unmanaged<CFDictionary>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateResourcePropertyForKeyFromBookmarkData(_ allocator: CFAllocator!, _ resourcePropertyKey: CFString!, _ bookmark: CFData!) -> Unmanaged<CFTypeRef>!
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateBookmarkDataFromFile(_ allocator: CFAllocator!, _ fileURL: CFURL!, _ errorRef: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFData>!
@available(OSX 10.6, *)
@discardableResult
func CFURLWriteBookmarkDataToFile(_ bookmarkRef: CFData!, _ fileURL: CFURL!, _ options: CFURLBookmarkFileCreationOptions, _ errorRef: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.6, *)
@discardableResult
func CFURLCreateBookmarkDataFromAliasRecord(_ allocatorRef: CFAllocator!, _ aliasRecordDataRef: CFData!) -> Unmanaged<CFData>!
@available(OSX 10.7, *)
@discardableResult
func CFURLStartAccessingSecurityScopedResource(_ url: CFURL!) -> Bool
@available(OSX 10.7, *)
func CFURLStopAccessingSecurityScopedResource(_ url: CFURL!)
