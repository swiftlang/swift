
@available(tvOS 5.0, *)
class AVMediaSelectionGroup : NSObject, NSCopying {
  var options: [AVMediaSelectionOption] { get }
  @available(tvOS 8.0, *)
  var defaultOption: AVMediaSelectionOption? { get }
  var allowsEmptySelection: Bool { get }
  @discardableResult
  func mediaSelectionOption(withPropertyList plist: AnyObject) -> AVMediaSelectionOption?
}
extension AVMediaSelectionGroup {
  @discardableResult
  class func playableMediaSelectionOptions(from mediaSelectionOptions: [AVMediaSelectionOption]) -> [AVMediaSelectionOption]
  @available(tvOS 6.0, *)
  @discardableResult
  class func mediaSelectionOptions(from mediaSelectionOptions: [AVMediaSelectionOption], filteredAndSortedAccordingToPreferredLanguages preferredLanguages: [String]) -> [AVMediaSelectionOption]
  @discardableResult
  class func mediaSelectionOptions(from mediaSelectionOptions: [AVMediaSelectionOption], with locale: NSLocale) -> [AVMediaSelectionOption]
  @discardableResult
  class func mediaSelectionOptions(from mediaSelectionOptions: [AVMediaSelectionOption], withMediaCharacteristics mediaCharacteristics: [String]) -> [AVMediaSelectionOption]
  @discardableResult
  class func mediaSelectionOptions(from mediaSelectionOptions: [AVMediaSelectionOption], withoutMediaCharacteristics mediaCharacteristics: [String]) -> [AVMediaSelectionOption]
}
@available(tvOS 5.0, *)
class AVMediaSelectionOption : NSObject, NSCopying {
  var mediaType: String { get }
  var mediaSubTypes: [NSNumber] { get }
  @discardableResult
  func hasMediaCharacteristic(_ mediaCharacteristic: String) -> Bool
  var isPlayable: Bool { get }
  @available(tvOS 7.0, *)
  var extendedLanguageTag: String? { get }
  var locale: NSLocale? { get }
  var commonMetadata: [AVMetadataItem] { get }
  var availableMetadataFormats: [String] { get }
  @discardableResult
  func metadata(forFormat format: String) -> [AVMetadataItem]
  @discardableResult
  func associatedMediaSelectionOption(in mediaSelectionGroup: AVMediaSelectionGroup) -> AVMediaSelectionOption?
  @discardableResult
  func propertyList() -> AnyObject
  @available(tvOS 7.0, *)
  @discardableResult
  func displayName(with locale: NSLocale) -> String
  @available(tvOS 7.0, *)
  var displayName: String { get }
}
