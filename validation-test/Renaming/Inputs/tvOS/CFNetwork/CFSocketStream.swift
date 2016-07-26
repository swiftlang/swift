
@available(tvOS 5.0, *)
let kCFStreamPropertySSLContext: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySSLPeerTrust: CFString
@available(tvOS 2.0, *)
let kCFStreamSSLValidatesCertificateChain: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySSLSettings: CFString
@available(tvOS 2.0, *)
let kCFStreamSSLLevel: CFString
@available(tvOS 2.0, *)
let kCFStreamSSLPeerName: CFString
@available(tvOS 2.0, *)
let kCFStreamSSLCertificates: CFString
@available(tvOS 2.0, *)
let kCFStreamSSLIsServer: CFString
@available(tvOS 4.0, *)
let kCFStreamNetworkServiceType: CFString
@available(tvOS 5.0, *)
let kCFStreamNetworkServiceTypeVideo: CFString
@available(tvOS 5.0, *)
let kCFStreamNetworkServiceTypeVoice: CFString
@available(tvOS 5.0, *)
let kCFStreamNetworkServiceTypeBackground: CFString
@available(tvOS, introduced: 4.0, deprecated: 9.0, message: "use PushKit for VoIP control purposes")
let kCFStreamNetworkServiceTypeVoIP: CFString
@available(tvOS 5.0, *)
let kCFStreamPropertyNoCellular: CFString
@available(tvOS 6.0, *)
let kCFStreamPropertyConnectionIsCellular: CFString
@available(tvOS 2.0, *)
let kCFStreamErrorDomainWinSock: CFIndex
@available(tvOS 2.0, *)
let kCFStreamErrorDomainSOCKS: Int32
@discardableResult
func CFSocketStreamSOCKSGetErrorSubdomain(_ error: UnsafePointer<CFStreamError>) -> Int32
@discardableResult
func CFSocketStreamSOCKSGetError(_ error: UnsafePointer<CFStreamError>) -> Int32
var kCFStreamErrorSOCKSSubDomainNone: Int { get }
var kCFStreamErrorSOCKSSubDomainVersionCode: Int { get }
var kCFStreamErrorSOCKS4SubDomainResponse: Int { get }
var kCFStreamErrorSOCKS5SubDomainUserPass: Int { get }
var kCFStreamErrorSOCKS5SubDomainMethod: Int { get }
var kCFStreamErrorSOCKS5SubDomainResponse: Int { get }
var kCFStreamErrorSOCKS5BadResponseAddr: Int { get }
var kCFStreamErrorSOCKS5BadState: Int { get }
var kCFStreamErrorSOCKSUnknownClientVersion: Int { get }
var kCFStreamErrorSOCKS4RequestFailed: Int { get }
var kCFStreamErrorSOCKS4IdentdFailed: Int { get }
var kCFStreamErrorSOCKS4IdConflict: Int { get }
var kSOCKS5NoAcceptableMethod: Int { get }
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSProxy: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSProxyHost: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSProxyPort: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSVersion: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSOCKSVersion4: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSOCKSVersion5: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSUser: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySOCKSPassword: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertyProxyLocalBypass: CFString
@available(tvOS 2.0, *)
let kCFStreamErrorDomainSSL: Int32
@available(tvOS 2.0, *)
let kCFStreamPropertySocketSecurityLevel: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSecurityLevelNone: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSecurityLevelSSLv2: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSecurityLevelSSLv3: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSecurityLevelTLSv1: CFString
@available(tvOS 2.0, *)
let kCFStreamSocketSecurityLevelNegotiatedSSL: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertyShouldCloseNativeSocket: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySocketRemoteHost: CFString
@available(tvOS 2.0, *)
let kCFStreamPropertySocketRemoteNetService: CFString
@available(tvOS 9.0, *)
let kCFStreamPropertySocketExtendedBackgroundIdleMode: CFString
@available(tvOS 2.0, *)
func CFStreamCreatePairWithSocketToCFHost(_ alloc: CFAllocator?, _ host: CFHost, _ port: Int32, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>?, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>?)
@available(tvOS 2.0, *)
func CFStreamCreatePairWithSocketToNetService(_ alloc: CFAllocator?, _ service: CFNetService, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>?, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>?)
