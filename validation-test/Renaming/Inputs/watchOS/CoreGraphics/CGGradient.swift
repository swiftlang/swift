
class CGGradient {
}
struct CGGradientDrawingOptions : OptionSet {
  init(rawValue rawValue: UInt32)
  let rawValue: UInt32
  static var drawsBeforeStartLocation: CGGradientDrawingOptions { get }
  static var drawsAfterEndLocation: CGGradientDrawingOptions { get }
}
extension CGGradient {
  @available(watchOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(watchOS 2.0, *)
  init?(withColorComponentsSpace space: CGColorSpace?, components components: UnsafePointer<CGFloat>?, locations locations: UnsafePointer<CGFloat>?, count count: Int)
  @available(watchOS 2.0, *)
  init?(withColorsSpace space: CGColorSpace?, colors colors: CFArray?, locations locations: UnsafePointer<CGFloat>?)
}
