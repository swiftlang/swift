
class CGColor {
}
extension CGColor {
  @available(iOS 2.0, *)
  /*not inherited*/ init?(withColorSpace space: CGColorSpace?, components components: UnsafePointer<CGFloat>?)
  @available(iOS 2.0, *)
  init?(withPatternSpace space: CGColorSpace?, pattern pattern: CGPattern?, components components: UnsafePointer<CGFloat>?)
  @available(iOS 2.0, *)
  init?(copy color: CGColor?)
  @available(iOS 2.0, *)
  init?(copyWithAlphaColor color: CGColor?, alpha alpha: CGFloat)
  @available(iOS 9.0, *)
  init?(copyByMatchingTo _: CGColorSpace?, intent intent: CGColorRenderingIntent, color color: CGColor?, options options: CFDictionary?)
  @available(iOS 2.0, *)
  @discardableResult
  func equalTo(_ color2: CGColor?) -> Bool
  @available(iOS 2.0, *)
  var numberOfComponents: Int { get }
  @available(iOS 2.0, *)
  var components: UnsafePointer<CGFloat>? { get }
  @available(iOS 2.0, *)
  var alpha: CGFloat { get }
  @available(iOS 2.0, *)
  var colorSpace: CGColorSpace? { get }
  @available(iOS 2.0, *)
  var pattern: CGPattern? { get }
  @available(iOS 2.0, *)
  class var typeID: CFTypeID { get }
}
