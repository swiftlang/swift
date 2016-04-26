
@available(OSX 10.6, *)
class NSTextInputContext : NSObject {
  @discardableResult
  class func current() -> NSTextInputContext?
  init(client theClient: NSTextInputClient)
  var client: NSTextInputClient { get }
  var acceptsGlyphInfo: Bool
  var allowedInputSourceLocales: [String]?
  func activate()
  func deactivate()
  @discardableResult
  func handle(_ theEvent: NSEvent) -> Bool
  func discardMarkedText()
  func invalidateCharacterCoordinates()
  var keyboardInputSources: [String]? { get }
  var selectedKeyboardInputSource: String?
  @discardableResult
  class func localizedName(forInputSource inputSourceIdentifier: String) -> String?
}
@available(OSX 10.6, *)
let NSTextInputContextKeyboardSelectionDidChangeNotification: String
