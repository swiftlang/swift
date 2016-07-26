
@available(tvOS 4.0, *)
class NSOrthography : NSObject, NSCopying, NSSecureCoding {
  var dominantScript: String { get }
  var languageMap: [String : [String]] { get }
  @available(tvOS 4.0, *)
  init(dominantScript script: String, languageMap map: [String : [String]])
}
extension NSOrthography {
  @available(tvOS 4.0, *)
  @discardableResult
  func languages(forScript script: String) -> [String]?
  @available(tvOS 4.0, *)
  @discardableResult
  func dominantLanguage(forScript script: String) -> String?
  @available(tvOS 4.0, *)
  var dominantLanguage: String { get }
  @available(tvOS 4.0, *)
  var allScripts: [String] { get }
  @available(tvOS 4.0, *)
  var allLanguages: [String] { get }
}
extension NSOrthography {
}
