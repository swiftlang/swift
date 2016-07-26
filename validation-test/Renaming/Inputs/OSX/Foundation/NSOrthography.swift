
@available(OSX 10.6, *)
class NSOrthography : NSObject, NSCopying, NSSecureCoding {
  var dominantScript: String { get }
  var languageMap: [String : [String]] { get }
  @available(OSX 10.6, *)
  init(dominantScript script: String, languageMap map: [String : [String]])
}
extension NSOrthography {
  @available(OSX 10.6, *)
  @discardableResult
  func languages(forScript script: String) -> [String]?
  @available(OSX 10.6, *)
  @discardableResult
  func dominantLanguage(forScript script: String) -> String?
  @available(OSX 10.6, *)
  var dominantLanguage: String { get }
  @available(OSX 10.6, *)
  var allScripts: [String] { get }
  @available(OSX 10.6, *)
  var allLanguages: [String] { get }
}
extension NSOrthography {
}
