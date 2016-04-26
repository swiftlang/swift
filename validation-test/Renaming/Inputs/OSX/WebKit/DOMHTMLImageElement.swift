
@available(OSX 10.4, *)
class DOMHTMLImageElement : DOMHTMLElement {
  var name: String!
  var align: String!
  var alt: String!
  var border: String!
  var height: Int32
  var hspace: Int32
  var isMap: Bool
  var longDesc: String!
  var src: String!
  var useMap: String!
  var vspace: Int32
  var width: Int32
  @available(OSX 10.5, *)
  var complete: Bool { get }
  @available(OSX 10.5, *)
  var lowsrc: String!
  @available(OSX 10.5, *)
  var naturalHeight: Int32 { get }
  @available(OSX 10.5, *)
  var naturalWidth: Int32 { get }
  @available(OSX 10.5, *)
  var x: Int32 { get }
  @available(OSX 10.5, *)
  var y: Int32 { get }
  @available(OSX 10.5, *)
  var altDisplayString: String! { get }
  @available(OSX 10.5, *)
  @NSCopying var absoluteImageURL: NSURL! { get }
}
