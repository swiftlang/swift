
class CGLayer {
}
extension CGLayer {
  @available(iOS 2.0, *)
  init?(with context: CGContext?, size size: CGSize, auxiliaryInfo auxiliaryInfo: CFDictionary?)
  @available(iOS 2.0, *)
  var size: CGSize { get }
  @available(iOS 2.0, *)
  var context: CGContext? { get }
  @available(iOS 2.0, *)
  class var typeID: CFTypeID { get }
}
extension CGContext {
  @available(iOS 2.0, *)
  func drawLayerInRect(_ rect: CGRect, layer layer: CGLayer?)
  @available(iOS 2.0, *)
  func drawLayerAtPoint(_ point: CGPoint, layer layer: CGLayer?)
}
