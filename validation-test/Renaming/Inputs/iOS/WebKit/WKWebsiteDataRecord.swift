
@available(iOS 9.0, *)
let WKWebsiteDataTypeDiskCache: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeMemoryCache: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeOfflineWebApplicationCache: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeCookies: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeSessionStorage: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeLocalStorage: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeWebSQLDatabases: String
@available(iOS 9.0, *)
let WKWebsiteDataTypeIndexedDBDatabases: String
@available(iOS 9.0, *)
class WKWebsiteDataRecord : NSObject {
  var displayName: String { get }
  var dataTypes: Set<String> { get }
}
