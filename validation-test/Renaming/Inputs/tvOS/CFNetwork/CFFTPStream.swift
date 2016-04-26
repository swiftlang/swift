
@available(tvOS 2.0, *)
let kCFStreamErrorDomainFTP: Int32
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUserName: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPPassword: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUsePassiveMode: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPResourceSize: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFetchResourceInfo: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFileTransferOffset: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPAttemptPersistentConnection: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxy: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyHost: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPort: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyUser: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPassword: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceMode: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceName: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceOwner: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceGroup: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceLink: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceSize: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceType: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceModDate: CFString
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFReadStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFReadStream>
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFFTPCreateParsedResourceListing(_ alloc: CFAllocator?, _ buffer: UnsafePointer<UInt8>, _ bufferLength: CFIndex, _ parsed: UnsafeMutablePointer<Unmanaged<CFDictionary>?>?) -> CFIndex
@available(tvOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFWriteStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFWriteStream>
