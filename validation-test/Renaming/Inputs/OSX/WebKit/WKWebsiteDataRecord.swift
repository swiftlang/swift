
@available(OSX 10.11, *)
let WKWebsiteDataTypeDiskCache: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeMemoryCache: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeOfflineWebApplicationCache: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeCookies: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeSessionStorage: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeLocalStorage: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeWebSQLDatabases: String
@available(OSX 10.11, *)
let WKWebsiteDataTypeIndexedDBDatabases: String
@available(OSX 10.11, *)
class WKWebsiteDataRecord : NSObject {
  var displayName: String { get }
  var dataTypes: Set<String> { get }
}
