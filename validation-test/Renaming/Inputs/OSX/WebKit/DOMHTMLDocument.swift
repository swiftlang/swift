
@available(OSX 10.4, *)
class DOMHTMLDocument : DOMDocument {
  @available(OSX 10.5, *)
  var embeds: DOMHTMLCollection! { get }
  @available(OSX 10.5, *)
  var plugins: DOMHTMLCollection! { get }
  @available(OSX 10.5, *)
  var scripts: DOMHTMLCollection! { get }
  @available(OSX 10.5, *)
  var width: Int32 { get }
  @available(OSX 10.5, *)
  var height: Int32 { get }
  @available(OSX 10.5, *)
  var dir: String!
  @available(OSX 10.5, *)
  var designMode: String!
  @available(OSX 10.6, *)
  var compatMode: String! { get }
  @available(OSX 10.5, *)
  var bgColor: String!
  @available(OSX 10.5, *)
  var fgColor: String!
  @available(OSX 10.5, *)
  var alinkColor: String!
  @available(OSX 10.5, *)
  var linkColor: String!
  @available(OSX 10.5, *)
  var vlinkColor: String!
  func open()
  func close()
  func write(_ text: String!)
  func writeln(_ text: String!)
  @available(OSX 10.6, *)
  func clear()
  @available(OSX 10.5, *)
  func captureEvents()
  @available(OSX 10.5, *)
  func releaseEvents()
}
