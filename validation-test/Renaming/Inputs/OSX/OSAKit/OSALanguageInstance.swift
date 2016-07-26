
class OSALanguageInstance : NSObject {
  init(language language: OSALanguage)
  var language: OSALanguage { get }
  var componentInstance: ComponentInstance { get }
  @available(OSX 10.10, *)
  var defaultTarget: NSAppleEventDescriptor?
  @available(OSX 10.10, *)
  @discardableResult
  func richText(from descriptor: NSAppleEventDescriptor) -> NSAttributedString?
}
