
class CGLayer {
}
extension CGLayer {
  @available(OSX 10.4, *)
  init?(with context: CGContext?, size size: CGSize, auxiliaryInfo auxiliaryInfo: CFDictionary?)
  @available(OSX 10.4, *)
  var size: CGSize { get }
  @available(OSX 10.4, *)
  var context: CGContext? { get }
  @available(OSX 10.4, *)
  class var typeID: CFTypeID { get }
}
extension CGContext {
  @available(OSX 10.4, *)
  func drawLayerInRect(_ rect: CGRect, layer layer: CGLayer?)
  @available(OSX 10.4, *)
  func drawLayerAtPoint(_ point: CGPoint, layer layer: CGLayer?)
}
