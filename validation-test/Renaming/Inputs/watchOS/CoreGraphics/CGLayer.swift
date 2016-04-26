
class CGLayer {
}
extension CGLayer {
  @available(watchOS 2.0, *)
  init?(with context: CGContext?, size size: CGSize, auxiliaryInfo auxiliaryInfo: CFDictionary?)
  @available(watchOS 2.0, *)
  var size: CGSize { get }
  @available(watchOS 2.0, *)
  var context: CGContext? { get }
  @available(watchOS 2.0, *)
  class var typeID: CFTypeID { get }
}
extension CGContext {
  @available(watchOS 2.0, *)
  func drawLayerInRect(_ rect: CGRect, layer layer: CGLayer?)
  @available(watchOS 2.0, *)
  func drawLayerAtPoint(_ point: CGPoint, layer layer: CGLayer?)
}
