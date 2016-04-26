
@available(OSX 10.4, *)
class DOMHTMLObjectElement : DOMHTMLElement {
  var form: DOMHTMLFormElement! { get }
  var code: String!
  var align: String!
  var archive: String!
  var border: String!
  var codeBase: String!
  var codeType: String!
  var data: String!
  var declare: Bool
  var height: String!
  var hspace: Int32
  var name: String!
  var standby: String!
  var type: String!
  var useMap: String!
  var vspace: Int32
  var width: String!
  var contentDocument: DOMDocument! { get }
  @available(OSX 10.5, *)
  @NSCopying var absoluteImageURL: NSURL! { get }
}
