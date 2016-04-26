
@available(iOS 2.0, *)
@discardableResult
func CFNetworkCopySystemProxySettings() -> Unmanaged<CFDictionary>?
@available(iOS 2.0, *)
@discardableResult
func CFNetworkCopyProxiesForURL(_ url: CFURL, _ proxySettings: CFDictionary) -> Unmanaged<CFArray>
typealias CFProxyAutoConfigurationResultCallback = @convention(c) (UnsafeMutablePointer<Void>, CFArray, CFError?) -> Void
@available(iOS 2.0, *)
@discardableResult
func CFNetworkCopyProxiesForAutoConfigurationScript(_ proxyAutoConfigurationScript: CFString, _ targetURL: CFURL, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Unmanaged<CFArray>?
@available(iOS 2.0, *)
@discardableResult
func CFNetworkExecuteProxyAutoConfigurationScript(_ proxyAutoConfigurationScript: CFString, _ targetURL: CFURL, _ cb: CFProxyAutoConfigurationResultCallback, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>) -> Unmanaged<CFRunLoopSource>
@available(iOS 2.0, *)
@discardableResult
func CFNetworkExecuteProxyAutoConfigurationURL(_ proxyAutoConfigURL: CFURL, _ targetURL: CFURL, _ cb: CFProxyAutoConfigurationResultCallback, _ clientContext: UnsafeMutablePointer<CFStreamClientContext>) -> Unmanaged<CFRunLoopSource>
@available(iOS 2.0, *)
let kCFProxyTypeKey: CFString
@available(iOS 2.0, *)
let kCFProxyHostNameKey: CFString
@available(iOS 2.0, *)
let kCFProxyPortNumberKey: CFString
@available(iOS 2.0, *)
let kCFProxyAutoConfigurationURLKey: CFString
@available(iOS 3.0, *)
let kCFProxyAutoConfigurationJavaScriptKey: CFString
@available(iOS 2.0, *)
let kCFProxyUsernameKey: CFString
@available(iOS 2.0, *)
let kCFProxyPasswordKey: CFString
@available(iOS 2.0, *)
let kCFProxyTypeNone: CFString
@available(iOS 2.0, *)
let kCFProxyTypeHTTP: CFString
@available(iOS 2.0, *)
let kCFProxyTypeHTTPS: CFString
@available(iOS 2.0, *)
let kCFProxyTypeSOCKS: CFString
@available(iOS 2.0, *)
let kCFProxyTypeFTP: CFString
@available(iOS 2.0, *)
let kCFProxyTypeAutoConfigurationURL: CFString
@available(iOS 3.0, *)
let kCFProxyTypeAutoConfigurationJavaScript: CFString
@available(iOS 2.0, *)
let kCFProxyAutoConfigurationHTTPResponseKey: CFString
@available(iOS 2.0, *)
let kCFNetworkProxiesHTTPEnable: CFString
@available(iOS 2.0, *)
let kCFNetworkProxiesHTTPPort: CFString
@available(iOS 2.0, *)
let kCFNetworkProxiesHTTPProxy: CFString
@available(iOS 2.0, *)
let kCFNetworkProxiesProxyAutoConfigEnable: CFString
@available(iOS 2.0, *)
let kCFNetworkProxiesProxyAutoConfigURLString: CFString
@available(iOS 3.0, *)
let kCFNetworkProxiesProxyAutoConfigJavaScript: CFString
