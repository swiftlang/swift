
typealias DOMTimeStamp = UInt64
@available(OSX 10.4, *)
class DOMObject : WebScriptObject, NSCopying {
}
extension DOMObject {
  @available(OSX 10.4, *)
  var sheet: DOMStyleSheet! { get }
}
