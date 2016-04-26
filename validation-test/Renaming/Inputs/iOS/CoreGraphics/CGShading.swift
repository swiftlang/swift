
class CGShading {
}
extension CGShading {
  @available(iOS 2.0, *)
  class var typeID: CFTypeID { get }
  @available(iOS 2.0, *)
  init?(axialSpace space: CGColorSpace?, start start: CGPoint, end end: CGPoint, function function: CGFunction?, extendStart extendStart: Bool, extendEnd extendEnd: Bool)
  @available(iOS 2.0, *)
  init?(radialSpace space: CGColorSpace?, start start: CGPoint, startRadius startRadius: CGFloat, end end: CGPoint, endRadius endRadius: CGFloat, function function: CGFunction?, extendStart extendStart: Bool, extendEnd extendEnd: Bool)
}
