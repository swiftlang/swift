
let kIMKCommandMenuItemName: String
let kIMKCommandClientName: String
extension NSObject {
  @discardableResult
  class func inputText(_ string: String!, key keyCode: Int, modifiers flags: Int, client sender: AnyObject!) -> Bool
  @discardableResult
  func inputText(_ string: String!, key keyCode: Int, modifiers flags: Int, client sender: AnyObject!) -> Bool
  @discardableResult
  class func inputText(_ string: String!, client sender: AnyObject!) -> Bool
  @discardableResult
  func inputText(_ string: String!, client sender: AnyObject!) -> Bool
  @discardableResult
  class func handle(_ event: NSEvent!, client sender: AnyObject!) -> Bool
  @discardableResult
  func handle(_ event: NSEvent!, client sender: AnyObject!) -> Bool
  @discardableResult
  class func didCommand(by aSelector: Selector!, client sender: AnyObject!) -> Bool
  @discardableResult
  func didCommand(by aSelector: Selector!, client sender: AnyObject!) -> Bool
  @discardableResult
  class func composedString(_ sender: AnyObject!) -> AnyObject!
  @discardableResult
  func composedString(_ sender: AnyObject!) -> AnyObject!
  @discardableResult
  class func originalString(_ sender: AnyObject!) -> NSAttributedString!
  @discardableResult
  func originalString(_ sender: AnyObject!) -> NSAttributedString!
  class func commitComposition(_ sender: AnyObject!)
  func commitComposition(_ sender: AnyObject!)
  @discardableResult
  class func candidates(_ sender: AnyObject!) -> [AnyObject]!
  @discardableResult
  func candidates(_ sender: AnyObject!) -> [AnyObject]!
}
protocol IMKStateSetting {
  func activateServer(_ sender: AnyObject!)
  func deactivateServer(_ sender: AnyObject!)
  @discardableResult
  func value(forTag tag: Int, client sender: AnyObject!) -> AnyObject!
  func setValue(_ value: AnyObject!, forTag tag: Int, client sender: AnyObject!)
  @available(OSX 10.0, *)
  @discardableResult
  func modes(_ sender: AnyObject!) -> [NSObject : AnyObject]!
  @discardableResult
  func recognizedEvents(_ sender: AnyObject!) -> Int
  func showPreferences(_ sender: AnyObject!)
}
protocol IMKMouseHandling {
  @discardableResult
  func mouseDown(onCharacterIndex index: Int, coordinate point: NSPoint, withModifier flags: Int, continueTracking keepTracking: UnsafeMutablePointer<ObjCBool>!, client sender: AnyObject!) -> Bool
  @discardableResult
  func mouseUp(onCharacterIndex index: Int, coordinate point: NSPoint, withModifier flags: Int, client sender: AnyObject!) -> Bool
  @discardableResult
  func mouseMoved(onCharacterIndex index: Int, coordinate point: NSPoint, withModifier flags: Int, client sender: AnyObject!) -> Bool
}
class IMKInputController : NSObject, IMKStateSetting, IMKMouseHandling {
  init!(server server: IMKServer!, delegate delegate: AnyObject!, client inputClient: AnyObject!)
  func updateComposition()
  func cancelComposition()
  @discardableResult
  func compositionAttributes(at range: NSRange) -> NSMutableDictionary!
  @discardableResult
  func selectionRange() -> NSRange
  @discardableResult
  func replacementRange() -> NSRange
  @discardableResult
  func mark(forStyle style: Int, at range: NSRange) -> [NSObject : AnyObject]!
  func doCommand(by aSelector: Selector!, command infoDictionary: [NSObject : AnyObject]!)
  func hidePalettes()
  @discardableResult
  func menu() -> NSMenu!
  @discardableResult
  func delegate() -> AnyObject!
  func setDelegate(_ newDelegate: AnyObject!)
  @discardableResult
  func server() -> IMKServer!
  @discardableResult
  func client() -> protocol<IMKTextInput, NSObjectProtocol>!
  @available(OSX 10.7, *)
  func inputControllerWillClose()
  func annotationSelected(_ annotationString: NSAttributedString!, forCandidate candidateString: NSAttributedString!)
  func candidateSelectionChanged(_ candidateString: NSAttributedString!)
  func candidateSelected(_ candidateString: NSAttributedString!)
}
