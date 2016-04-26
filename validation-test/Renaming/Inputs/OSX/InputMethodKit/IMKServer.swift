
let IMKModeDictionary: String
let IMKControllerClass: String
let IMKDelegateClass: String
class IMKServer : NSObject {
  init!(name name: String!, bundleIdentifier bundleIdentifier: String!)
  init!(name name: String!, controllerClass controllerClassID: AnyClass!, delegateClass delegateClassID: AnyClass!)
  @discardableResult
  func bundle() -> NSBundle!
  @available(OSX 10.7, *)
  @discardableResult
  func paletteWillTerminate() -> Bool
  @available(OSX 10.7, *)
  @discardableResult
  func lastKeyEventWasDeadKey() -> Bool
}
