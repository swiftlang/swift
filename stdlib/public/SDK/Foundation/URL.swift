//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

/// Keys used in the result of `URLResourceValues.thumbnailDictionary`.
@available(OSX 10.10, iOS 8.0, *)
public struct URLThumbnailSizeKey : RawRepresentable, Hashable {
    public typealias RawValue = String
    
    public init(rawValue: RawValue) { self.rawValue = rawValue }
    private(set) public var rawValue: RawValue
    
    /// Key for a 1024 x 1024 thumbnail image.
    static public let none: URLThumbnailSizeKey = URLThumbnailSizeKey(rawValue: URLThumbnailDictionaryItem.NSThumbnail1024x1024SizeKey.rawValue)
  
    public var hashValue: Int {
        return rawValue.hashValue
    }
}

/**
 URLs to file system resources support the properties defined below. Note that not all property values will exist for all file system URLs. For example, if a file is located on a volume that does not support creation dates, it is valid to request the creation date property, but the returned value will be nil, and no error will be generated.
 
 Only the fields requested by the keys you pass into the `URL` function to receive this value will be populated. The others will return `nil` regardless of the underlying property on the file system.
 
 As a convenience, volume resource values can be requested from any file system URL. The value returned will reflect the property value for the volume on which the resource is located.
*/
public struct URLResourceValues {
    private var _values: [URLResourceKey: AnyObject]
    private var _keys: Set<URLResourceKey>
    
    public init() {
        _values = [:]
        _keys = []
    }
    
    private init(keys: Set<URLResourceKey>, values: [URLResourceKey: AnyObject]) {
        _values = values
        _keys = keys
    }
    
    private func contains(_ key: URLResourceKey) -> Bool {
        return _keys.contains(key)
    }
    
    private func _get<T>(_ key : URLResourceKey) -> T? {
        return _values[key] as? T
    }
    private func _get(_ key : URLResourceKey) -> Bool? {
        return (_values[key] as? NSNumber)?.boolValue
    }
    private func _get(_ key: URLResourceKey) -> Int? {
        return (_values[key] as? NSNumber)?.intValue
    }
    
    private mutating func _set(_ key : URLResourceKey, newValue : AnyObject?) {
        _keys.insert(key)
        _values[key] = newValue
    }
    private mutating func _set(_ key : URLResourceKey, newValue : String?) {
        _keys.insert(key)
        _values[key] = newValue as NSString?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : [String]?) {
        _keys.insert(key)
        _values[key] = newValue as NSObject?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Date?) {
        _keys.insert(key)
        _values[key] = newValue as NSDate?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : URL?) {
        _keys.insert(key)
        _values[key] = newValue as NSURL?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Bool?) {
        _keys.insert(key)
        if let value = newValue {
            _values[key] = NSNumber(value: value)
        } else {
            _values[key] = nil
        }
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Int?) {
        _keys.insert(key)
        if let value = newValue {
            _values[key] = NSNumber(value: value)
        } else {
            _values[key] = nil
        }
    }
    
    /// A loosely-typed dictionary containing all keys and values.
    ///
    /// If you have set temporary keys or non-standard keys, you can find them in here.
    public var allValues : [URLResourceKey : AnyObject] {
        return _values
    }
    
    /// The resource name provided by the file system.
    public var name: String? {
        get { return _get(.nameKey) }
        set { _set(.nameKey, newValue: newValue) }
    }
    
    /// Localized or extension-hidden name as displayed to users.
    public var localizedName: String? { return _get(.localizedNameKey) }
    
    /// True for regular files.
    public var isRegularFile: Bool? { return _get(.isRegularFileKey) }
    
    /// True for directories.
    public var isDirectory: Bool? { return _get(.isDirectoryKey) }
    
    /// True for symlinks.
    public var isSymbolicLink: Bool? { return _get(.isSymbolicLinkKey) }
    
    /// True for the root directory of a volume.
    public var isVolume: Bool? { return _get(.isVolumeKey) }
    
    /// True for packaged directories. 
    ///
    /// - note: You can only set or clear this property on directories; if you try to set this property on non-directory objects, the property is ignored. If the directory is a package for some other reason (extension type, etc), setting this property to false will have no effect.
    public var isPackage: Bool? {
        get { return _get(.isPackageKey) }
        set { _set(.isPackageKey, newValue: newValue) }
    }
    
    /// True if resource is an application.
    @available(OSX 10.11, iOS 9.0, *)
    public var isApplication: Bool? { return _get(.isApplicationKey) }
    
#if os(OSX)
    /// True if the resource is scriptable. Only applies to applications.
    @available(OSX 10.11, *)
    public var applicationIsScriptable: Bool? { return _get(.applicationIsScriptableKey) }
#endif
    
    /// True for system-immutable resources.
    public var isSystemImmutable: Bool? { return _get(.isSystemImmutableKey) }
    
    /// True for user-immutable resources
    public var isUserImmutable: Bool? {
        get { return _get(.isUserImmutableKey) }
        set { _set(.isUserImmutableKey, newValue: newValue) }
    }
    
    /// True for resources normally not displayed to users.
    /// 
    /// - note: If the resource is a hidden because its name starts with a period, setting this property to false will not change the property.
    public var isHidden: Bool? {
        get { return _get(.isHiddenKey) }
        set { _set(.isHiddenKey, newValue: newValue) }
    }
    
    /// True for resources whose filename extension is removed from the localized name property.
    public var hasHiddenExtension: Bool? {
        get { return _get(.hasHiddenExtensionKey) }
        set { _set(.hasHiddenExtensionKey, newValue: newValue) }
    }
    
    /// The date the resource was created.
    public var creationDate: Date? {
        get { return _get(.creationDateKey) }
        set { _set(.creationDateKey, newValue: newValue) }
    }
    
    /// The date the resource was last accessed.
    public var contentAccessDate: Date? {
        get { return _get(.contentAccessDateKey) }
        set { _set(.contentAccessDateKey, newValue: newValue) }
    }
    
    /// The time the resource content was last modified.
    public var contentModificationDate: Date? {
        get { return _get(.contentModificationDateKey) }
        set { _set(.contentModificationDateKey, newValue: newValue) }
    }
    
    /// The time the resource's attributes were last modified.
    public var attributeModificationDate: Date? { return _get(.attributeModificationDateKey) }
    
    /// Number of hard links to the resource.
    public var linkCount: Int? { return _get(.linkCountKey) }
    
    /// The resource's parent directory, if any.
    public var parentDirectory: URL? { return _get(.parentDirectoryURLKey) }
    
    /// URL of the volume on which the resource is stored.
    public var volume: URL? { return _get(.volumeURLKey) }
    
    /// Uniform type identifier (UTI) for the resource.
    public var typeIdentifier: String? { return _get(.typeIdentifierKey) }
    
    /// User-visible type or "kind" description.
    public var localizedTypeDescription: String? { return _get(.localizedTypeDescriptionKey) }
    
    /// The label number assigned to the resource.
    public var labelNumber: Int? {
        get { return _get(.labelNumberKey) }
        set { _set(.labelNumberKey, newValue: newValue) }
    }
    
    
    /// The user-visible label text.
    public var localizedLabel: String? {
        get { return _get(.localizedLabelKey) }
    }
        
    /// An identifier which can be used to compare two file system objects for equality using `isEqual`.
    ///
    /// Two object identifiers are equal if they have the same file system path or if the paths are linked to same inode on the same file system. This identifier is not persistent across system restarts.
    public var fileResourceIdentifier: protocol<NSCopying, NSCoding, NSSecureCoding, NSObjectProtocol>? { return _get(.fileResourceIdentifierKey) }
    
    /// An identifier that can be used to identify the volume the file system object is on. 
    ///
    /// Other objects on the same volume will have the same volume identifier and can be compared using for equality using `isEqual`. This identifier is not persistent across system restarts.
    public var volumeIdentifier: protocol<NSCopying, NSCoding, NSSecureCoding, NSObjectProtocol>? { return _get(.volumeIdentifierKey) }
    
    /// The optimal block size when reading or writing this file's data, or nil if not available.
    public var preferredIOBlockSize: Int? { return _get(.preferredIOBlockSizeKey) }
    
    /// True if this process (as determined by EUID) can read the resource.
    public var isReadable: Bool? { return _get(.isReadableKey) }
    
    /// True if this process (as determined by EUID) can write to the resource.
    public var isWritable: Bool? { return _get(.isWritableKey) }
    
    /// True if this process (as determined by EUID) can execute a file resource or search a directory resource.
    public var isExecutable: Bool? { return _get(.isExecutableKey) }
    
    /// The file system object's security information encapsulated in a FileSecurity object.
    public var fileSecurity: NSFileSecurity? {
        get { return _get(.fileSecurityKey) }
        set { _set(.fileSecurityKey, newValue: newValue) }
    }
    
    /// True if resource should be excluded from backups, false otherwise.
    ///
    /// This property is only useful for excluding cache and other application support files which are not needed in a backup. Some operations commonly made to user documents will cause this property to be reset to false and so this property should not be used on user documents.
    public var isExcludedFromBackup: Bool? {
        get { return _get(.isExcludedFromBackupKey) }
        set { _set(.isExcludedFromBackupKey, newValue: newValue) }
    }
    
#if os(OSX)
    /// The array of Tag names.
    public var tagNames: [String]? { return _get(.tagNamesKey) }
#endif
    /// The URL's path as a file system path.
    public var path: String? { return _get(.pathKey) }
    
    /// The URL's path as a canonical absolute file system path.
    @available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var canonicalPath: String? { return _get(.canonicalPathKey) }
    
    /// True if this URL is a file system trigger directory. Traversing or opening a file system trigger will cause an attempt to mount a file system on the trigger directory.
    public var isMountTrigger: Bool? { return _get(.isMountTriggerKey) }
    
    /// An opaque generation identifier which can be compared using `==` to determine if the data in a document has been modified.
    ///
    /// For URLs which refer to the same file inode, the generation identifier will change when the data in the file's data fork is changed (changes to extended attributes or other file system metadata do not change the generation identifier). For URLs which refer to the same directory inode, the generation identifier will change when direct children of that directory are added, removed or renamed (changes to the data of the direct children of that directory will not change the generation identifier). The generation identifier is persistent across system restarts. The generation identifier is tied to a specific document on a specific volume and is not transferred when the document is copied to another volume. This property is not supported by all volumes.
    @available(OSX 10.10, iOS 8.0, *)
    public var generationIdentifier: protocol<NSCopying, NSCoding, NSSecureCoding, NSObjectProtocol>? { return _get(.generationIdentifierKey) }
    
    /// The document identifier -- a value assigned by the kernel to a document (which can be either a file or directory) and is used to identify the document regardless of where it gets moved on a volume.
    ///
    /// The document identifier survives "safe save" operations; i.e it is sticky to the path it was assigned to (`replaceItem(at:,withItemAt:,backupItemName:,options:,resultingItem:) throws` is the preferred safe-save API). The document identifier is persistent across system restarts. The document identifier is not transferred when the file is copied. Document identifiers are only unique within a single volume. This property is not supported by all volumes.
    @available(OSX 10.10, iOS 8.0, *)
    public var documentIdentifier: Int? { return _get(.documentIdentifierKey) }
    
    /// The date the resource was created, or renamed into or within its parent directory. Note that inconsistent behavior may be observed when this attribute is requested on hard-linked items. This property is not supported by all volumes.
    @available(OSX 10.10, iOS 8.0, *)
    public var addedToDirectoryDate: Date? { return _get(.addedToDirectoryDateKey) }
    
#if os(OSX)
    /// The quarantine properties as defined in LSQuarantine.h. To remove quarantine information from a file, pass `nil` as the value when setting this property.
    @available(OSX 10.10, *)
    public var quarantineProperties: [String : AnyObject]? {
        get { return _get(.quarantinePropertiesKey) }
        set { _set(.quarantinePropertiesKey, newValue: newValue as NSObject?) }
    }
#endif
    
    /// Returns the file system object type.
    public var fileResourceType: URLFileResourceType? { return _get(.fileResourceTypeKey) }
    
    /// The user-visible volume format.
    public var volumeLocalizedFormatDescription : String? { return _get(.volumeLocalizedFormatDescriptionKey) }
    
    /// Total volume capacity in bytes.
    public var volumeTotalCapacity : Int? { return _get(.volumeTotalCapacityKey) }
    
    /// Total free space in bytes.
    public var volumeAvailableCapacity : Int? { return _get(.volumeAvailableCapacityKey) }
    
    /// Total number of resources on the volume.
    public var volumeResourceCount : Int? { return _get(.volumeResourceCountKey) }
    
    /// true if the volume format supports persistent object identifiers and can look up file system objects by their IDs.
    public var volumeSupportsPersistentIDs : Bool? { return _get(.volumeSupportsPersistentIDsKey) }
    
    /// true if the volume format supports symbolic links.
    public var volumeSupportsSymbolicLinks : Bool? { return _get(.volumeSupportsSymbolicLinksKey) }
    
    /// true if the volume format supports hard links.
    public var volumeSupportsHardLinks : Bool? { return _get(.volumeSupportsHardLinksKey) }
    
    /// true if the volume format supports a journal used to speed recovery in case of unplanned restart (such as a power outage or crash). This does not necessarily mean the volume is actively using a journal. 
    public var volumeSupportsJournaling : Bool? { return _get(.volumeSupportsJournalingKey) }
    
    /// true if the volume is currently using a journal for speedy recovery after an unplanned restart. 
    public var volumeIsJournaling : Bool? { return _get(.volumeIsJournalingKey) }
    
    /// true if the volume format supports sparse files, that is, files which can have 'holes' that have never been written to, and thus do not consume space on disk. A sparse file may have an allocated size on disk that is less than its logical length.
    public var volumeSupportsSparseFiles : Bool? { return _get(.volumeSupportsSparseFilesKey) }
    
    /// For security reasons, parts of a file (runs) that have never been written to must appear to contain zeroes. true if the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media. 
    public var volumeSupportsZeroRuns : Bool? { return _get(.volumeSupportsZeroRunsKey) }

    /// true if the volume format treats upper and lower case characters in file and directory names as different. Otherwise an upper case character is equivalent to a lower case character, and you can't have two names that differ solely in the case of the characters. 
    public var volumeSupportsCaseSensitiveNames : Bool? { return _get(.volumeSupportsCaseSensitiveNamesKey) }

    /// true if the volume format preserves the case of file and directory names.  Otherwise the volume may change the case of some characters (typically making them all upper or all lower case). 
    public var volumeSupportsCasePreservedNames : Bool? { return _get(.volumeSupportsCasePreservedNamesKey) }

    /// true if the volume supports reliable storage of times for the root directory. 
    public var volumeSupportsRootDirectoryDates : Bool? { return _get(.volumeSupportsRootDirectoryDatesKey) }

    /// true if the volume supports returning volume size values (`volumeTotalCapacity` and `volumeAvailableCapacity`).
    public var volumeSupportsVolumeSizes : Bool? { return _get(.volumeSupportsVolumeSizesKey) }

    /// true if the volume can be renamed. 
    public var volumeSupportsRenaming : Bool? { return _get(.volumeSupportsRenamingKey) }

    /// true if the volume implements whole-file flock(2) style advisory locks, and the O_EXLOCK and O_SHLOCK flags of the open(2) call. 
    public var volumeSupportsAdvisoryFileLocking : Bool? { return _get(.volumeSupportsAdvisoryFileLockingKey) }

    /// true if the volume implements extended security (ACLs). 
    public var volumeSupportsExtendedSecurity : Bool? { return _get(.volumeSupportsExtendedSecurityKey) }

    /// true if the volume should be visible via the GUI (i.e., appear on the Desktop as a separate volume). 
    public var volumeIsBrowsable : Bool? { return _get(.volumeIsBrowsableKey) }

    /// The largest file size (in bytes) supported by this file system, or nil if this cannot be determined. 
    public var volumeMaximumFileSize : Int? { return _get(.volumeMaximumFileSizeKey) }

    /// true if the volume's media is ejectable from the drive mechanism under software control. 
    public var volumeIsEjectable : Bool? { return _get(.volumeIsEjectableKey) }

    /// true if the volume's media is removable from the drive mechanism. 
    public var volumeIsRemovable : Bool? { return _get(.volumeIsRemovableKey) }

    /// true if the volume's device is connected to an internal bus, false if connected to an external bus, or nil if not available. 
    public var volumeIsInternal : Bool? { return _get(.volumeIsInternalKey) }

    /// true if the volume is automounted. Note: do not mistake this with the functionality provided by kCFURLVolumeSupportsBrowsingKey. 
    public var volumeIsAutomounted : Bool? { return _get(.volumeIsAutomountedKey) }

    /// true if the volume is stored on a local device. 
    public var volumeIsLocal : Bool? { return _get(.volumeIsLocalKey) }

    /// true if the volume is read-only. 
    public var volumeIsReadOnly : Bool? { return _get(.volumeIsReadOnlyKey) }

    /// The volume's creation date, or nil if this cannot be determined. 
    public var volumeCreationDate : Date? { return _get(.volumeCreationDateKey) }

    /// The `URL` needed to remount a network volume, or nil if not available.
    public var volumeURLForRemounting : URL? { return _get(.volumeURLForRemountingKey) }

    /// The volume's persistent `UUID` as a string, or nil if a persistent `UUID` is not available for the volume.
    public var volumeUUIDString : String? { return _get(.volumeUUIDStringKey) }

    /// The name of the volume 
    public var volumeName : String? {
        get { return _get(.volumeNameKey) }
        set { _set(.volumeNameKey, newValue: newValue) }
    }
    
    /// The user-presentable name of the volume 
    public var volumeLocalizedName : String? { return _get(.volumeLocalizedNameKey) }
    
    /// true if the volume is encrypted. 
    @available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeIsEncrypted : Bool? { return _get(.volumeIsEncryptedKey) }

    /// true if the volume is the root filesystem. 
    @available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeIsRootFileSystem : Bool? { return _get(.volumeIsRootFileSystemKey) }

    /// true if the volume supports transparent decompression of compressed files using decmpfs. 
    @available(OSX 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeSupportsCompression : Bool? { return _get(.volumeSupportsCompressionKey) }
    
    /// true if this item is synced to the cloud, false if it is only a local file. 
    public var isUbiquitousItem : Bool? { return _get(.isUbiquitousItemKey) }

    /// true if this item has conflicts outstanding. 
    public var ubiquitousItemHasUnresolvedConflicts : Bool? { return _get(.ubiquitousItemHasUnresolvedConflictsKey) }

    /// true if data is being downloaded for this item. 
    public var ubiquitousItemIsDownloading : Bool? { return _get(.ubiquitousItemIsDownloadingKey) }

    /// true if there is data present in the cloud for this item. 
    public var ubiquitousItemIsUploaded : Bool? { return _get(.ubiquitousItemIsUploadedKey) }

    /// true if data is being uploaded for this item. 
    public var ubiquitousItemIsUploading : Bool? { return _get(.ubiquitousItemIsUploadingKey) }
    
    /// returns the download status of this item.
    public var ubiquitousItemDownloadingStatus : URLUbiquitousItemDownloadingStatus? { return _get(.ubiquitousItemDownloadingStatusKey) }
    
    /// returns the error when downloading the item from iCloud failed, see the NSUbiquitousFile section in FoundationErrors.h
    public var ubiquitousItemDownloadingError : NSError? { return _get(.ubiquitousItemDownloadingErrorKey) }

    /// returns the error when uploading the item to iCloud failed, see the NSUbiquitousFile section in FoundationErrors.h
    public var ubiquitousItemUploadingError : NSError? { return _get(.ubiquitousItemUploadingErrorKey) }
    
    /// returns whether a download of this item has already been requested with an API like `startDownloadingUbiquitousItem(at:) throws`.
    @available(OSX 10.10, iOS 8.0, *)
    public var ubiquitousItemDownloadRequested : Bool? { return _get(.ubiquitousItemDownloadRequestedKey) }
    
    /// returns the name of this item's container as displayed to users.
    @available(OSX 10.10, iOS 8.0, *)
    public var ubiquitousItemContainerDisplayName : String? { return _get(.ubiquitousItemContainerDisplayNameKey) }
    
#if !os(OSX)
    /// The protection level for this file
    @available(iOS 9.0, *)
    public var fileProtection : URLFileProtection? { return _get(.fileProtectionKey) }
#endif
    
    /// Total file size in bytes
    ///
    /// - note: Only applicable to regular files.
    public var fileSize : Int? { return _get(.fileSizeKey) }
    
    /// Total size allocated on disk for the file in bytes (number of blocks times block size)
    ///
    /// - note: Only applicable to regular files.
    public var fileAllocatedSize : Int? { return _get(.fileAllocatedSizeKey) }
    
    /// Total displayable size of the file in bytes (this may include space used by metadata), or nil if not available.
    ///
    /// - note: Only applicable to regular files.
    public var totalFileSize : Int? { return _get(.totalFileSizeKey) }
    
    /// Total allocated size of the file in bytes (this may include space used by metadata), or nil if not available. This can be less than the value returned by `totalFileSize` if the resource is compressed.
    ///
    /// - note: Only applicable to regular files.
    public var totalFileAllocatedSize : Int? { return _get(.totalFileAllocatedSizeKey) }

    /// true if the resource is a Finder alias file or a symlink, false otherwise
    ///
    /// - note: Only applicable to regular files.
    public var isAliasFile : Bool? { return _get(.isAliasFileKey) }

}

/**
 A URL is a type that can potentially contain the location of a resource on a remote server, the path of a local file on disk, or even an arbitrary piece of encoded data.
 
 You can construct URLs and access their parts. For URLs that represent local files, you can also manipulate properties of those files directly, such as changing the file's last modification date. Finally, you can pass URLs to other APIs to retrieve the contents of those URLs. For example, you can use the URLSession classes to access the contents of remote resources, as described in URL Session Programming Guide.
 
 URLs are the preferred way to refer to local files. Most objects that read data from or write data to a file have methods that accept a URL instead of a pathname as the file reference. For example, you can get the contents of a local file URL as `String` by calling `func init(contentsOf:encoding) throws`, or as a `Data` by calling `func init(contentsOf:options) throws`.
*/
public struct URL : ReferenceConvertible, CustomStringConvertible, Equatable {
    public typealias ReferenceType = NSURL
    private var _url : NSURL
    
    public typealias BookmarkResolutionOptions = NSURL.BookmarkResolutionOptions
    public typealias BookmarkCreationOptions = NSURL.BookmarkCreationOptions

    /// Initialize with string.
    ///
    /// Returns `nil` if a `URL` cannot be formed with the string.
    public init?(string: String) {
        if let inner = NSURL(string: string) {
            _url = inner
        } else {
            return nil
        }
    }
    
    /// Initialize with string, relative to another URL.
    ///
    /// Returns `nil` if a `URL` cannot be formed with the string.
    public init?(string: String, relativeTo url: URL) {
        if let inner = NSURL(string: string, relativeTo: url) {
            _url = inner
        } else {
            return nil
        }
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path, relative to a base URL.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    /// - note: This function avoids an extra file system access to check if the file URL is a directory. You should use it if you know the answer already.
    @available(OSX 10.11, iOS 9.0, *)
    public init(fileURLWithPath path: String, isDirectory: Bool, relativeTo base: URL?) {
        _url = NSURL(fileURLWithPath: path.isEmpty ? "." : path, isDirectory: isDirectory, relativeTo: base)
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path, relative to a base URL.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    @available(OSX 10.11, iOS 9.0, *)
    public init(fileURLWithPath path: String, relativeTo base: URL?) {
        _url = NSURL(fileURLWithPath: path.isEmpty ? "." : path, relativeTo: base)
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    /// - note: This function avoids an extra file system access to check if the file URL is a directory. You should use it if you know the answer already.
    public init(fileURLWithPath path: String, isDirectory: Bool) {
        _url = NSURL(fileURLWithPath: path.isEmpty ? "." : path, isDirectory: isDirectory)
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    public init(fileURLWithPath path: String) {
        _url = NSURL(fileURLWithPath: path.isEmpty ? "." : path)
    }
    
    /// Initializes a newly created URL using the contents of the given data, relative to a base URL. If the data representation is not a legal URL string as ASCII bytes, the URL object may not behave as expected.
    @available(OSX 10.11, iOS 9.0, *)
    public init(dataRepresentation: Data, relativeTo url: URL?, isAbsolute: Bool = false) {
        if isAbsolute {
            _url = NSURL(absoluteURLWithDataRepresentation: dataRepresentation, relativeTo: url)
        } else {
            _url = NSURL(dataRepresentation: dataRepresentation, relativeTo: url)
        }
    }

    /// Initializes a URL that refers to a location specified by resolving bookmark data.
    public init?(resolvingBookmarkData data: Data, options: BookmarkResolutionOptions = [], relativeTo url: URL? = nil, bookmarkDataIsStale: inout Bool) throws {
        var stale : ObjCBool = false
        _url = try NSURL(resolvingBookmarkData: data, options: options, relativeTo: url, bookmarkDataIsStale: &stale)
        bookmarkDataIsStale = stale.boolValue
    }
    
    /// Creates and initializes a NSURL that refers to the location specified by resolving the alias file at url. If the url argument does not refer to an alias file as defined by the NSURLIsAliasFileKey property, the NSURL returned is the same as url argument. This method fails and returns nil if the url argument is unreachable, or if the original file or directory could not be located or is not reachable, or if the original file or directory is on a volume that could not be located or mounted. The URLBookmarkResolutionWithSecurityScope option is not supported by this method.
    @available(OSX 10.10, iOS 8.0, *)
    public init(resolvingAliasFileAt url: URL, options: BookmarkResolutionOptions = []) throws {
        _url = try NSURL(resolvingAliasFileAt: url, options: options)
    }

    /// Initializes a newly created URL referencing the local file or directory at the file system representation of the path. File system representation is a null-terminated C string with canonical UTF-8 encoding.
    public init(fileURLWithFileSystemRepresentation path: UnsafePointer<Int8>, isDirectory: Bool, relativeToURL baseURL: URL?) {
        _url = NSURL(fileURLWithFileSystemRepresentation: path, isDirectory: isDirectory, relativeTo: baseURL)
    }
    
    // MARK: -
    
    public var description: String {
        return _url.description
    }

    public var debugDescription: String {
        return _url.debugDescription
    }

    public var hashValue : Int {
        return _url.hash
    }
    
    // MARK: -
    
    /// Returns the data representation of the URL's relativeString. If the URL was initialized with -initWithData:relativeToURL:, the data representation returned are the same bytes as those used at initialization; otherwise, the data representation returned are the bytes of the relativeString encoded with NSUTF8StringEncoding.
    @available(OSX 10.11, iOS 9.0, *)
    public var dataRepresentation: Data { return _url.dataRepresentation as Data }
    
    public var absoluteString: String? { return _url.absoluteString }
    
    /// The relative portion of a URL.  If baseURL is nil, or if the receiver is itself absolute, this is the same as absoluteString
    public var relativeString: String { return _url.relativeString }
    public var baseURL: URL? { return _url.baseURL }
    
    /// If the receiver is itself absolute, this will return self.
    public var absoluteURL: URL? {  return _url.absoluteURL }
    
    /// Any URL is composed of these two basic pieces.  The full URL would be the concatenation of `myURL.scheme, ':', myURL.resourceSpecifier`.
    public var scheme: String? { return _url.scheme }
    
    /// Any URL is composed of these two basic pieces.  The full URL would be the concatenation of `myURL.scheme, ':', myURL.resourceSpecifier`.
    public var resourceSpecifier: String? { return _url.resourceSpecifier }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var host: String? { return _url.host }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var port: Int? { return _url.port?.intValue }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var user: String? { return _url.user }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var password: String? { return _url.password }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var path: String? { return _url.path }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var fragment: String? { return _url.fragment }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var parameterString: String? { return _url.parameterString }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var query: String? { return _url.query }
    
    /// If the URL conforms to rfc 1808 (the most common form of URL), returns a component of the URL; otherwise it returns nil.
    ///
    /// This is the same as path if baseURL is nil.
    /// The litmus test for conformance is as recommended in RFC 1808 - whether the first two characters of resourceSpecifier is "//".  In all cases, they return the component's value after resolving the receiver against its base URL.
    public var relativePath: String? { return _url.relativePath }
    
    @available(OSX 10.11, iOS 9.0, *)
    public var hasDirectoryPath: Bool { return _url.hasDirectoryPath }
    
    /// Passes the URL's path in file system representation to `block`. 
    /// 
    /// File system representation is a null-terminated C string with canonical UTF-8 encoding.
    /// - note: The pointer is not valid outside the context of the block.
    @available(OSX 10.9, iOS 7.0, *)
    public func withUnsafeFileSystemRepresentation(_ block: @noescape (UnsafePointer<Int8>) throws -> Void) rethrows {
        try block(_url.fileSystemRepresentation)
    }
    
    /// Whether the scheme is file:; if `myURL.isFileURL` is `true`, then `myURL.path` is suitable for input into `FileManager` or `PathUtilities`.
    public var isFileURL: Bool {
        return _url.isFileURL
    }
    
    public func standardized() throws -> URL {
        if let result = _url.standardized.map({ URL(reference: $0 as NSURL) }) {
            return result;
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    
    /// Returns a file reference URL that refers to the same resource as a specified file URL.
    ///
    /// File reference URLs use a URL path syntax that identifies a file system object by reference, not by path. This form of file URL remains valid when the file system path of the URL's underlying resource changes. An error will occur if the url parameter is not a file URL. File reference URLs cannot be created to file system objects which do not exist or are not reachable. In some areas of the file system hierarchy, file reference URLs cannot be generated to the leaf node of the URL path. A file reference URL's path should never be persistently stored because is not valid across system restarts, and across remounts of volumes -- if you want to create a persistent reference to a file system object, use a bookmark.
    /// - seealso: func bookmarkData(options: BookmarkCreationOptions = [], includingResourceValuesForKeys keys: Set<URLResourceKey>? = nil, relativeTo url: URL? = nil) throws -> Data
    public func fileReferenceURL() throws -> URL {
        if let result = _url.fileReferenceURL().map({ URL(reference: $0 as NSURL) }) {
            return result as URL
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    
    /// Returns `true` if the URL is a file reference URL.
    public var isFileReferenceURL : Bool {
        return _url.isFileReferenceURL()
    }
    
    /// Returns a file path URL that refers to the same resource as a specified URL. 
    ///
    /// File path URLs use a file system style path. A file reference URL's resource must exist and be reachable to be converted to a file path URL.
    public func filePathURL() throws -> URL {
        if let result = _url.filePathURL.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    
    public var pathComponents: [String]? { return _url.pathComponents }
    
    public var lastPathComponent: String? { return _url.lastPathComponent }
    
    public var pathExtension: String? { return _url.pathExtension }
    
    public func appendingPathComponent(_ pathComponent: String, isDirectory: Bool) throws -> URL {
        // TODO: Use URLComponents to handle an empty-path case
        /*
         URLByAppendingPathComponent can return nil if:
         * the URL does not have a path component. (see note 1)
         * a mutable copy of the URLs string could not be created.
         * a percent-encoded string of the new path component could not created using the same encoding as the URL's string. (see note 2)
         * a new URL object could not be created with the modified URL string.
         
         Note 1: If NS/CFURL parsed URLs correctly, this would not occur because URL strings always have a path component. For example, the URL <mailto:user@example.com> should be parsed as Scheme="mailto", and Path= "user@example.com". Instead, CFURL returns false for CFURLCanBeDecomposed(), says Scheme="mailto", Path=nil, and ResourceSpecifier="user@example.com". rdar://problem/15060399
         
         Note 2: CFURLCreateWithBytes() and CFURLCreateAbsoluteURLWithBytes() allow URLs to be created with an array of bytes and a CFStringEncoding. All other CFURL functions and URL methods which create URLs use kCFStringEncodingUTF8/NSUTF8StringEncoding. So, the encoding passed to CFURLCreateWithBytes/CFURLCreateAbsoluteURLWithBytes might prevent the percent-encoding of the new path component or path extension.
         */
        guard let result = _url.appendingPathComponent(pathComponent, isDirectory: isDirectory) else {
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
        return result
    }
    public mutating func appendPathComponent(_ pathComponent: String, isDirectory: Bool) throws {
        self = try appendingPathComponent(pathComponent, isDirectory: isDirectory)
    }
    
    public func appendingPathComponent(_ pathComponent: String) throws -> URL {
        guard let result = _url.appendingPathComponent(pathComponent) else {
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
        return result
    }
    public mutating func appendPathComponent(_ pathComponent: String) throws {
        self = try appendingPathComponent(pathComponent)        
    }

    public func deletingLastPathComponent() throws -> URL {
        /*
         URLByDeletingLastPathComponent can return nil if:
         * the URL is a file reference URL which cannot be resolved back to a path.
         * the URL does not have a path component. (see note 1)
         * a mutable copy of the URLs string could not be created.
         * a new URL object could not be created with the modified URL string.
         */
        if let result = _url.deletingLastPathComponent.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    public mutating func deleteLastPathComponent() throws {
        let result = try deletingLastPathComponent()
        self = result
    }
    
    public func appendingPathExtension(_ pathExtension: String) throws -> URL {
        /*
         URLByAppendingPathExtension can return nil if:
         * the new path extension is not a valid extension (see _CFExtensionIsValidToAppend)
         * the URL is a file reference URL which cannot be resolved back to a path.
         * the URL does not have a path component. (see note 1)
         * a mutable copy of the URLs string could not be created.
         * a percent-encoded string of the new path extension could not created using the same encoding as the URL's string. (see note 1))
         * a new URL object could not be created with the modified URL string.
         */
        guard let result = _url.appendingPathExtension(pathExtension) else {
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
        return result
    }
    public mutating func appendPathExtension(_ pathExtension: String) throws {
        self = try appendingPathExtension(pathExtension)
    }
    
    public func deletingPathExtension() throws -> URL {
        /*
         URLByDeletingPathExtension can return nil if:
         * the URL is a file reference URL which cannot be resolved back to a path.
         * the URL does not have a path component. (see note 1)
         * a mutable copy of the URLs string could not be created.
         * a new URL object could not be created with the modified URL string.
         */
        if let result = _url.deletingPathExtension.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    public mutating func deletePathExtension() throws {
        let result = try deletingPathExtension()
        self = result
    }
    
    public func standardizingPath() throws -> URL {
        /*
         URLByStandardizingPath can return nil if:
         * the URL is a file reference URL which cannot be resolved back to a path.
         * a new URL object could not be created with the standardized path).
         */
        if let result = _url.standardizingPath.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }
    public mutating func standardizePath() throws {
        let result = try standardizingPath()
        self = result
    }
    
    public func resolvingSymlinksInPath() throws -> URL {
        /*
         URLByResolvingSymlinksInPath can return nil if:
         * the URL is a file reference URL which cannot be resolved back to a path.
         * NSPathUtilities' stringByResolvingSymlinksInPath property returns nil.
         * a new URL object could not be created with the resolved path).
         */
        if let result = _url.resolvingSymlinksInPath.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            // TODO: We need to call into CFURL to figure out the error
            throw NSError(domain: NSCocoaErrorDomain, code: NSFileReadUnknownError, userInfo: [:])
        }
    }

    public mutating func resolveSymlinksInPath() throws {
        let result = try resolvingSymlinksInPath()
        self = result
    }

    // MARK: - Reachability
    
    /// Returns whether the URL's resource exists and is reachable. 
    ///
    /// This method synchronously checks if the resource's backing store is reachable. Checking reachability is appropriate when making decisions that do not require other immediate operations on the resource, e.g. periodic maintenance of UI state that depends on the existence of a specific document. When performing operations such as opening a file or copying resource properties, it is more efficient to simply try the operation and handle failures. This method is currently applicable only to URLs for file system resources. For other URL types, `false` is returned.
    public func checkResourceIsReachable() throws -> Bool {
        var error : NSError? = nil
        let result = _url.checkResourceIsReachableAndReturnError(&error)
        if let e = error {
            throw e
        } else {
            return result
        }
    }
    
    /// Returns whether the promised item URL's resource exists and is reachable.
    ///
    /// This method synchronously checks if the resource's backing store is reachable. Checking reachability is appropriate when making decisions that do not require other immediate operations on the resource, e.g. periodic maintenance of UI state that depends on the existence of a specific document. When performing operations such as opening a file or copying resource properties, it is more efficient to simply try the operation and handle failures. This method is currently applicable only to URLs for file system resources. For other URL types, `false` is returned.
    @available(OSX 10.10, iOS 8.0, *)
    public func checkPromisedItemIsReachable() throws -> Bool {
        var error : NSError? = nil
        let result = _url.checkPromisedItemIsReachableAndReturnError(&error)
        if let e = error {
            throw e
        } else {
            return result
        }
    }
    
    // MARK: - Resource Values
    
    /// Sets the resource value identified by a given resource key.
    /// 
    /// This method writes the new resource values out to the backing store. Attempts to set a read-only resource property or to set a resource property not supported by the resource are ignored and are not considered errors. This method is currently applicable only to URLs for file system resources.
    ///
    /// `URLResourceValues` keeps track of which of its properties have been set. Those values are the ones used by this function to determine which properties to write.
    public mutating func setResourceValues(_ values: URLResourceValues) throws {
        try _url.setResourceValues(values._values)
    }
    
    /// Return a collection of resource values identified by the given resource keys.
    ///
    /// This method first checks if the URL object already caches the resource value. If so, it returns the cached resource value to the caller. If not, then this method synchronously obtains the resource value from the backing store, adds the resource value to the URL object's cache, and returns the resource value to the caller. The type of the resource value varies by resource property (see resource key definitions). If this method does not throw and the resulting value in the `URLResourceValues` is populated with nil, it means the resource property is not available for the specified resource and no errors occurred when determining the resource property was not available. This method is currently applicable only to URLs for file system resources.
    ///
    /// When this function is used from the main thread, resource values cached by the URL (except those added as temporary properties) are removed the next time the main thread's run loop runs. `func removeCachedResourceValue(forKey:)` and `func removeAllCachedResourceValues()` also may be used to remove cached resource values.
    ///
    /// Only the values for the keys specified in `keys` will be populated.
    public func resourceValues(forKeys keys: Set<URLResourceKey>) throws -> URLResourceValues {
        return URLResourceValues(keys: keys, values: try _url.resourceValues(forKeys: Array(keys)))
    }

    /// Sets a temporary resource value on the URL object.
    ///
    /// Temporary resource values are for client use. Temporary resource values exist only in memory and are never written to the resource's backing store. Once set, a temporary resource value can be copied from the URL object with `func resourceValues(forKeys:)`. The values are stored in the loosely-typed `allValues` dictionary property.
    ///
    /// To remove a temporary resource value from the URL object, use `func removeCachedResourceValue(forKey:)`. Care should be taken to ensure the key that identifies a temporary resource value is unique and does not conflict with system defined keys (using reverse domain name notation in your temporary resource value keys is recommended). This method is currently applicable only to URLs for file system resources.
    public mutating func setTemporaryResourceValue(_ value : AnyObject, forKey key: URLResourceKey) {
        _url.setTemporaryResourceValue(value, forKey: key)
    }
    
    /// Removes all cached resource values and all temporary resource values from the URL object.
    /// 
    /// This method is currently applicable only to URLs for file system resources.
    public mutating func removeAllCachedResourceValues() {
        _url.removeAllCachedResourceValues()
    }
    
    /// Removes the cached resource value identified by a given resource value key from the URL object.
    /// 
    /// Removing a cached resource value may remove other cached resource values because some resource values are cached as a set of values, and because some resource values depend on other resource values (temporary resource values have no dependencies). This method is currently applicable only to URLs for file system resources.
    public mutating func removeCachedResourceValue(forKey key: URLResourceKey) {
        _url.removeCachedResourceValue(forKey: key)
    }
    
    /// Get resource values from URLs of 'promised' items.
    ///
    /// A promised item is not guaranteed to have its contents in the file system until you use `FileCoordinator` to perform a coordinated read on its URL, which causes the contents to be downloaded or otherwise generated. Promised item URLs are returned by various APIs, including currently:
    ///     NSMetadataQueryUbiquitousDataScope
    ///     NSMetadataQueryUbiquitousDocumentsScope
    ///     A `FilePresenter` presenting the contents of the directory located by -URLForUbiquitousContainerIdentifier: or a subdirectory thereof
    ///
    /// The following methods behave identically to their similarly named methods above (`func resourceValues(forKeys:)`, etc.), except that they allow you to get resource values and check for presence regardless of whether the promised item's contents currently exist at the URL. You must use these APIs instead of the normal URL resource value APIs if and only if any of the following are true:
    ///     You are using a URL that you know came directly from one of the above APIs
    ///     You are inside the accessor block of a coordinated read or write that used NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly, NSFileCoordinatorWritingForDeleting, NSFileCoordinatorWritingForMoving, or NSFileCoordinatorWritingContentIndependentMetadataOnly
    ///
    /// Most of the URL resource value keys will work with these APIs. However, there are some that are tied to the item's contents that will not work, such as `contentAccessDateKey` or `generationIdentifierKey`. If one of these keys is used, the method will return a `URLResourceValues` value, but the value for that property will be nil.
    @available(OSX 10.10, iOS 8.0, *)
    public func promisedItemResourceValues(forKeys keys: Set<URLResourceKey>) throws -> URLResourceValues {
        return URLResourceValues(keys: keys, values: try _url.promisedItemResourceValues(forKeys: Array(keys)))
    }
    
    // MARK: - Bookmarks and Alias Files
    
    /// Returns bookmark data for the URL, created with specified options and resource values.
    public func bookmarkData(options: BookmarkCreationOptions = [], includingResourceValuesForKeys keys: Set<URLResourceKey>? = nil, relativeTo url: URL? = nil) throws -> Data {
        let result = try _url.bookmarkData(options: options, includingResourceValuesForKeys: keys.flatMap { Array($0) }, relativeTo: url)
        return result as Data
    }
    
    /// Returns the resource values for properties identified by a specified array of keys contained in specified bookmark data. If the result dictionary does not contain a resource value for one or more of the requested resource keys, it means those resource properties are not available in the bookmark data.
    public static func resourceValues(forKeys keys: Set<URLResourceKey>, fromBookmarkData data: Data) -> URLResourceValues? {
        return NSURL.resourceValues(forKeys: Array(keys), fromBookmarkData: data).map { URLResourceValues(keys: keys, values: $0) }
    }
    
    /// Creates an alias file on disk at a specified location with specified bookmark data. bookmarkData must have been created with the URLBookmarkCreationSuitableForBookmarkFile option. bookmarkFileURL must either refer to an existing file (which will be overwritten), or to location in an existing directory.
    public static func writeBookmarkData(_ data : Data, to url: URL) throws {
        // Options are unused
        try NSURL.writeBookmarkData(data, to: url, options: 0)
    }
 
    /// Initializes and returns bookmark data derived from an alias file pointed to by a specified URL. If bookmarkFileURL refers to an alias file created prior to OS X v10.6 that contains Alias Manager information but no bookmark data, this method synthesizes bookmark data for the file.
    public static func bookmarkData(withContentsOf url: URL) throws -> Data {
        let result = try NSURL.bookmarkData(withContentsOf: url)
        return result as Data
    }
    
    /// Given a NSURL created by resolving a bookmark data created with security scope, make the resource referenced by the url accessible to the process. When access to this resource is no longer needed the client must call stopAccessingSecurityScopedResource. Each call to startAccessingSecurityScopedResource must be balanced with a call to stopAccessingSecurityScopedResource (Note: this is not reference counted).
    @available(OSX 10.7, iOS 8.0, *)
    public func startAccessingSecurityScopedResource() -> Bool {
        return _url.startAccessingSecurityScopedResource()
    }
    
    /// Revokes the access granted to the url by a prior successful call to startAccessingSecurityScopedResource.
    @available(OSX 10.7, iOS 8.0, *)
    public func stopAccessingSecurityScopedResource() {
        _url.stopAccessingSecurityScopedResource()
    }
    
    // MARK: - Bridging Support
    
    private init(reference: NSURL) {
        _url = reference.copy() as! NSURL
    }
    
    private var reference : NSURL {
        return _url
    }
}

public func ==(lhs: URL, rhs: URL) -> Bool {
    return lhs.reference.isEqual(rhs.reference)
}

extension URL : _ObjectiveCBridgeable {
    public static func _isBridgedToObjectiveC() -> Bool {
        return true
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSURL {
        return _url
    }
    
    public static func _forceBridgeFromObjectiveC(_ x: NSURL, result: inout URL?) {
        if !_conditionallyBridgeFromObjectiveC(x, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ x: NSURL, result: inout URL?) -> Bool {
        result = URL(reference: x)
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSURL?) -> URL {
        var result: URL? = nil
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension URL : CustomPlaygroundQuickLookable {
    public var customPlaygroundQuickLook: PlaygroundQuickLook {
        if let str = absoluteString {
            return .url(str)
        } else {
            return .text(self.description)
        }
    }
}

//===----------------------------------------------------------------------===//
// File references, for playgrounds.
//===----------------------------------------------------------------------===//

extension URL : _FileReferenceLiteralConvertible {
  public init(fileReferenceLiteralResourceName name: String) {
    self = Bundle.main().urlForResource(name, withExtension: nil)!
  }
}

public typealias _FileReferenceLiteralType = URL
