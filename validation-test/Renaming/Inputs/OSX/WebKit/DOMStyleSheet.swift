
@available(OSX 10.4, *)
class DOMStyleSheet : DOMObject {
  var type: String! { get }
  var disabled: Bool
  var ownerNode: DOMNode! { get }
  var parent: DOMStyleSheet! { get }
  var href: String! { get }
  var title: String! { get }
  var media: DOMMediaList! { get }
}
