
@available(OSX 10.5, *)
class CBIdentityPicker : NSObject {
  var title: String?
  var allowsMultipleSelection: Bool
  @discardableResult
  func runModal() -> Int
  @available(OSX, introduced: 10.5, deprecated: 10.11)
  func runModal(for window: NSWindow, modalDelegate delegate: AnyObject?, didEnd didEndSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func runModal(for window: NSWindow, completionHandler completionHandler: ((NSModalResponse) -> Void)? = nil)
  var identities: [CBIdentity] { get }
}
