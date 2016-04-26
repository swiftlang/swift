
class CGShading {
}
extension CGShading {
  @available(OSX 10.2, *)
  class var typeID: CFTypeID { get }
  @available(OSX 10.2, *)
  init?(axialSpace space: CGColorSpace?, start start: CGPoint, end end: CGPoint, function function: CGFunction?, extendStart extendStart: Bool, extendEnd extendEnd: Bool)
  @available(OSX 10.2, *)
  init?(radialSpace space: CGColorSpace?, start start: CGPoint, startRadius startRadius: CGFloat, end end: CGPoint, endRadius endRadius: CGFloat, function function: CGFunction?, extendStart extendStart: Bool, extendEnd extendEnd: Bool)
}
