
let OSAScriptErrorMessageKey: String
let OSAScriptErrorBriefMessageKey: String
let OSAScriptErrorNumberKey: String
let OSAScriptErrorPartialResultKey: String
let OSAScriptErrorOffendingObjectKey: String
let OSAScriptErrorExpectedTypeKey: String
let OSAScriptErrorAppAddressKey: String
let OSAScriptErrorAppNameKey: String
let OSAScriptErrorRangeKey: String
let OSAScriptErrorMessage: String
let OSAScriptErrorNumber: String
let OSAScriptErrorAppName: String
let OSAScriptErrorBriefMessage: String
let OSAScriptErrorRange: String
let OSAStorageScriptType: String
let OSAStorageScriptBundleType: String
let OSAStorageApplicationType: String
let OSAStorageApplicationBundleType: String
let OSAStorageTextType: String
struct OSAStorageOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var preventGetSource: OSAStorageOptions { get }
  static var compileIntoContext: OSAStorageOptions { get }
  static var dontSetScriptLocation: OSAStorageOptions { get }
  static var stayOpenApplet: OSAStorageOptions { get }
  static var showStartupScreen: OSAStorageOptions { get }
}
class OSAScript : NSObject, NSCopying {
  @available(OSX 10.6, *)
  @discardableResult
  class func scriptDataDescriptor(withContentsOf url: NSURL) -> NSAppleEventDescriptor?
  init(source source: String)
  init(source source: String, language language: OSALanguage?)
  @available(OSX 10.6, *)
  init(source source: String, from url: NSURL?, languageInstance instance: OSALanguageInstance?, using storageOptions: OSAStorageOptions = [])
  init?(contentsOf url: NSURL, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?)
  @available(OSX 10.6, *)
  init(contentsOf url: NSURL, languageInstance instance: OSALanguageInstance?, using storageOptions: OSAStorageOptions = []) throws
  @available(OSX 10.6, *)
  init(compiledData data: NSData, from url: NSURL?, using storageOptions: OSAStorageOptions = []) throws
  @available(OSX 10.6, *)
  init(scriptDataDescriptor data: NSAppleEventDescriptor, from url: NSURL?, languageInstance instance: OSALanguageInstance?, using storageOptions: OSAStorageOptions = []) throws
  var source: String { get }
  @NSCopying var url: NSURL? { get }
  var language: OSALanguage
  @available(OSX 10.6, *)
  var languageInstance: OSALanguageInstance
  var isCompiled: Bool { get }
  @discardableResult
  func compileAndReturnError(_ errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
  @discardableResult
  func executeAndReturnError(_ errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor?
  @discardableResult
  func executeAppleEvent(_ event: NSAppleEventDescriptor, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor?
  @discardableResult
  func executeAndReturnDisplayValue(_ displayValue: AutoreleasingUnsafeMutablePointer<NSAttributedString?>, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor?
  @discardableResult
  func executeHandler(withName name: String, arguments arguments: [AnyObject], error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSAppleEventDescriptor?
  @NSCopying var richTextSource: NSAttributedString? { get }
  @discardableResult
  func richText(from descriptor: NSAppleEventDescriptor) -> NSAttributedString?
  @discardableResult
  func write(to url: NSURL, ofType type: String, error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
  @discardableResult
  func write(to url: NSURL, ofType type: String, using storageOptions: OSAStorageOptions = [], error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> Bool
  @discardableResult
  func compiledData(forType type: String, using storageOptions: OSAStorageOptions = [], error errorInfo: AutoreleasingUnsafeMutablePointer<NSDictionary?>?) -> NSData?
}
