
@available(OSX 10.6, *)
@discardableResult
func CFNetworkCopySystemProxySettings() -> Unmanaged<CFDictionary>?
@available(OSX 10.5, *)
@discardableResult
func CFNetworkCopyProxiesForURL(_ url: CFURL, _ proxySettings: CFDictionary) -> Unmanaged<CFArray>
typealias CFProxyAutoConfigurationResultCallback = @convention(c) (UnsafeMutablePointer<Void>, CFArray, CFError?) -> Void
@available(OSX 10.5, *)
@discardableResult
func CFNetworkCopyProxiesForAutoConfigurationScript(_ proxyAutoConfigurationScript: CFString, _ targetURL: CFURL, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Unmanaged<CFArray>?
@available(OSX 10.5, *)
@discardableResult
func CFNetworkExecuteProxyAutoConfigurationScript(_ proxyAutoConfigurationScript: CFString, _ targetURL: CFURL, _ cb: CFProxyAutoConfigurationResultCallback, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>) -> Unmanaged<CFRunLoopSource>
@available(OSX 10.5, *)
@discardableResult
func CFNetworkExecuteProxyAutoConfigurationURL(_ proxyAutoConfigURL: CFURL, _ targetURL: CFURL, _ cb: CFProxyAutoConfigurationResultCallback, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>) -> Unmanaged<CFRunLoopSource>
@available(OSX 10.5, *)
let kCFProxyTypeKey: CFString
@available(OSX 10.5, *)
let kCFProxyHostNameKey: CFString
@available(OSX 10.5, *)
let kCFProxyPortNumberKey: CFString
@available(OSX 10.5, *)
let kCFProxyAutoConfigurationURLKey: CFString
@available(OSX 10.7, *)
let kCFProxyAutoConfigurationJavaScriptKey: CFString
@available(OSX 10.5, *)
let kCFProxyUsernameKey: CFString
@available(OSX 10.5, *)
let kCFProxyPasswordKey: CFString
@available(OSX 10.5, *)
let kCFProxyTypeNone: CFString
@available(OSX 10.5, *)
let kCFProxyTypeHTTP: CFString
@available(OSX 10.5, *)
let kCFProxyTypeHTTPS: CFString
@available(OSX 10.5, *)
let kCFProxyTypeSOCKS: CFString
@available(OSX 10.5, *)
let kCFProxyTypeFTP: CFString
@available(OSX 10.5, *)
let kCFProxyTypeAutoConfigurationURL: CFString
@available(OSX 10.7, *)
let kCFProxyTypeAutoConfigurationJavaScript: CFString
@available(OSX 10.5, *)
let kCFProxyAutoConfigurationHTTPResponseKey: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesExceptionsList: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesExcludeSimpleHostnames: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesFTPEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesFTPPassive: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesFTPPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesFTPProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesGopherEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesGopherPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesGopherProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPSEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPSPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesHTTPSProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesRTSPEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesRTSPPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesRTSPProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesSOCKSEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesSOCKSPort: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesSOCKSProxy: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesProxyAutoConfigEnable: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesProxyAutoConfigURLString: CFString
@available(OSX 10.7, *)
let kCFNetworkProxiesProxyAutoConfigJavaScript: CFString
@available(OSX 10.6, *)
let kCFNetworkProxiesProxyAutoDiscoveryEnable: CFString
