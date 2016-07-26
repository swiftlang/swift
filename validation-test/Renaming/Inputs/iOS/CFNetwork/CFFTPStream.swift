
@available(iOS 2.0, *)
let kCFStreamErrorDomainFTP: Int32
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUserName: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPPassword: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUsePassiveMode: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPResourceSize: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFetchResourceInfo: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFileTransferOffset: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPAttemptPersistentConnection: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxy: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyHost: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPort: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyUser: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPassword: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceMode: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceName: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceOwner: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceGroup: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceLink: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceSize: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceType: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceModDate: CFString
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFReadStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFReadStream>
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFFTPCreateParsedResourceListing(_ alloc: CFAllocator?, _ buffer: UnsafePointer<UInt8>, _ bufferLength: CFIndex, _ parsed: UnsafeMutablePointer<Unmanaged<CFDictionary>?>?) -> CFIndex
@available(iOS, introduced: 2.0, deprecated: 9.0, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFWriteStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFWriteStream>
