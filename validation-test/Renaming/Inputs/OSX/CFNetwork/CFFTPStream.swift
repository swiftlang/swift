
@available(OSX 10.3, *)
let kCFStreamErrorDomainFTP: Int32
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUserName: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPPassword: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPUsePassiveMode: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPResourceSize: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFetchResourceInfo: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPFileTransferOffset: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPAttemptPersistentConnection: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxy: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyHost: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPort: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyUser: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFStreamPropertyFTPProxyPassword: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceMode: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceName: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceOwner: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceGroup: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceLink: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceSize: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceType: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
let kCFFTPResourceModDate: CFString
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFReadStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFReadStream>
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFFTPCreateParsedResourceListing(_ alloc: CFAllocator?, _ buffer: UnsafePointer<UInt8>, _ bufferLength: CFIndex, _ parsed: UnsafeMutablePointer<Unmanaged<CFDictionary>?>?) -> CFIndex
@available(OSX, introduced: 10.3, deprecated: 10.11, message: "Use NSURLSessionAPI for ftp requests")
@discardableResult
func CFWriteStreamCreateWithFTPURL(_ alloc: CFAllocator?, _ ftpURL: CFURL) -> Unmanaged<CFWriteStream>
