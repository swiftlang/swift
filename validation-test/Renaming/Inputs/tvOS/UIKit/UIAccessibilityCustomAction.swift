
@available(tvOS 8.0, *)
class UIAccessibilityCustomAction : NSObject {
  init(name name: String, target target: AnyObject?, selector selector: Selector)
  var name: String
  weak var target: @sil_weak AnyObject?
  var selector: Selector
}
