
@available(OSX 10.10, *)
class SKLightNode : SKNode {
  var isEnabled: Bool
  var lightColor: NSColor
  var ambientColor: NSColor
  var shadowColor: NSColor
  var falloff: CGFloat
  var categoryBitMask: UInt32
}
