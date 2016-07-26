
struct OSALanguageFeatures : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var supportsCompiling: OSALanguageFeatures { get }
  static var supportsGetSource: OSALanguageFeatures { get }
  static var supportsAECoercion: OSALanguageFeatures { get }
  static var supportsAESending: OSALanguageFeatures { get }
  static var supportsRecording: OSALanguageFeatures { get }
  static var supportsConvenience: OSALanguageFeatures { get }
  static var supportsDialects: OSALanguageFeatures { get }
  static var supportsEventHandling: OSALanguageFeatures { get }
}
class OSALanguage : NSObject {
  @discardableResult
  class func availableLanguages() -> [OSALanguage]
  /*not inherited*/ init?(forName name: String)
  @available(OSX 10.6, *)
  /*not inherited*/ init?(forScriptDataDescriptor descriptor: NSAppleEventDescriptor)
  @discardableResult
  class func defaultLanguage() -> OSALanguage?
  class func setDefaultLanguage(_ defaultLanguage: OSALanguage)
  init(component component: Component)
  @available(OSX 10.6, *)
  @discardableResult
  func sharedLanguageInstance() -> OSALanguageInstance
  var componentInstance: ComponentInstance { get }
  var name: String? { get }
  var info: String? { get }
  var version: String? { get }
  var type: OSType { get }
  var subType: OSType { get }
  var manufacturer: OSType { get }
  var features: OSALanguageFeatures { get }
  @available(OSX 10.6, *)
  var isThreadSafe: Bool { get }
}
