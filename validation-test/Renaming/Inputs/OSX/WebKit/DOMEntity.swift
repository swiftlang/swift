
@available(OSX 10.4, *)
class DOMEntity : DOMNode {
  var publicId: String! { get }
  var systemId: String! { get }
  var notationName: String! { get }
}
