
class NSBundle : NSObject {
  @discardableResult
  class func main() -> NSBundle
  init?(path path: String)
  @available(OSX 10.6, *)
  convenience init?(url url: NSURL)
  /*not inherited*/ init(for aClass: AnyClass)
  /*not inherited*/ init?(identifier identifier: String)
  @discardableResult
  class func allBundles() -> [NSBundle]
  @discardableResult
  class func allFrameworks() -> [NSBundle]
  @discardableResult
  func load() -> Bool
  var isLoaded: Bool { get }
  @discardableResult
  func unload() -> Bool
  @available(OSX 10.5, *)
  func preflight() throws
  @available(OSX 10.5, *)
  func loadAndReturnError() throws
  @available(OSX 10.6, *)
  @NSCopying var bundleURL: NSURL { get }
  @available(OSX 10.6, *)
  @NSCopying var resourceURL: NSURL? { get }
  @available(OSX 10.6, *)
  @NSCopying var executableURL: NSURL? { get }
  @available(OSX 10.6, *)
  @discardableResult
  func urlForAuxiliaryExecutable(_ executableName: String) -> NSURL?
  @available(OSX 10.6, *)
  @NSCopying var privateFrameworksURL: NSURL? { get }
  @available(OSX 10.6, *)
  @NSCopying var sharedFrameworksURL: NSURL? { get }
  @available(OSX 10.6, *)
  @NSCopying var sharedSupportURL: NSURL? { get }
  @available(OSX 10.6, *)
  @NSCopying var builtInPlugInsURL: NSURL? { get }
  @available(OSX 10.7, *)
  @NSCopying var appStoreReceiptURL: NSURL? { get }
  var bundlePath: String { get }
  var resourcePath: String? { get }
  var executablePath: String? { get }
  @discardableResult
  func pathForAuxiliaryExecutable(_ executableName: String) -> String?
  var privateFrameworksPath: String? { get }
  var sharedFrameworksPath: String? { get }
  var sharedSupportPath: String? { get }
  var builtInPlugInsPath: String? { get }
  @available(OSX 10.6, *)
  @discardableResult
  class func urlForResource(_ name: String?, withExtension ext: String?, subdirectory subpath: String?, inBundleWith bundleURL: NSURL) -> NSURL?
  @available(OSX 10.6, *)
  @discardableResult
  class func urlsForResources(withExtension ext: String?, subdirectory subpath: String?, inBundleWith bundleURL: NSURL) -> [NSURL]?
  @available(OSX 10.6, *)
  @discardableResult
  func urlForResource(_ name: String?, withExtension ext: String?) -> NSURL?
  @available(OSX 10.6, *)
  @discardableResult
  func urlForResource(_ name: String?, withExtension ext: String?, subdirectory subpath: String?) -> NSURL?
  @available(OSX 10.6, *)
  @discardableResult
  func urlForResource(_ name: String?, withExtension ext: String?, subdirectory subpath: String?, localization localizationName: String?) -> NSURL?
  @available(OSX 10.6, *)
  @discardableResult
  func urlsForResources(withExtension ext: String?, subdirectory subpath: String?) -> [NSURL]?
  @available(OSX 10.6, *)
  @discardableResult
  func urlsForResources(withExtension ext: String?, subdirectory subpath: String?, localization localizationName: String?) -> [NSURL]?
  @discardableResult
  class func pathForResource(_ name: String?, ofType ext: String?, inDirectory bundlePath: String) -> String?
  @discardableResult
  class func pathsForResources(ofType ext: String?, inDirectory bundlePath: String) -> [String]
  @discardableResult
  func pathForResource(_ name: String?, ofType ext: String?) -> String?
  @discardableResult
  func pathForResource(_ name: String?, ofType ext: String?, inDirectory subpath: String?) -> String?
  @discardableResult
  func pathForResource(_ name: String?, ofType ext: String?, inDirectory subpath: String?, forLocalization localizationName: String?) -> String?
  @discardableResult
  func pathsForResources(ofType ext: String?, inDirectory subpath: String?) -> [String]
  @discardableResult
  func pathsForResources(ofType ext: String?, inDirectory subpath: String?, forLocalization localizationName: String?) -> [String]
  @discardableResult
  func localizedString(forKey key: String, value value: String?, table tableName: String?) -> String
  var bundleIdentifier: String? { get }
  var infoDictionary: [String : AnyObject]? { get }
  var localizedInfoDictionary: [String : AnyObject]? { get }
  @discardableResult
  func objectForInfoDictionaryKey(_ key: String) -> AnyObject?
  @discardableResult
  func classNamed(_ className: String) -> AnyClass?
  var principalClass: AnyClass? { get }
  var preferredLocalizations: [String] { get }
  var localizations: [String] { get }
  var developmentLocalization: String? { get }
  @discardableResult
  class func preferredLocalizations(from localizationsArray: [String]) -> [String]
  @discardableResult
  class func preferredLocalizations(from localizationsArray: [String], forPreferences preferencesArray: [String]?) -> [String]
  @available(OSX 10.5, *)
  var executableArchitectures: [NSNumber]? { get }
}
var NSBundleExecutableArchitectureI386: Int { get }
var NSBundleExecutableArchitecturePPC: Int { get }
var NSBundleExecutableArchitectureX86_64: Int { get }
var NSBundleExecutableArchitecturePPC64: Int { get }
extension NSString {
  @available(OSX 10.11, *)
  @discardableResult
  func variantFittingPresentationWidth(_ width: Int) -> String
}
let NSBundleDidLoadNotification: String
let NSLoadedClasses: String
extension NSBundle {
}
