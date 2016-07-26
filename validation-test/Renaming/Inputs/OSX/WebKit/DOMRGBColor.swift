
@available(OSX 10.4, *)
class DOMRGBColor : DOMObject {
  var red: DOMCSSPrimitiveValue! { get }
  var green: DOMCSSPrimitiveValue! { get }
  var blue: DOMCSSPrimitiveValue! { get }
  var alpha: DOMCSSPrimitiveValue! { get }
  @available(OSX 10.5, *)
  @NSCopying var color: NSColor! { get }
}
