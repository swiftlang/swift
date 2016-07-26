
@available(tvOS 9.0, *)
class MDLObject : NSObject, MDLNamed {
  func setComponent(_ component: MDLComponent, for protocol: Protocol)
  @discardableResult
  func componentConforming(to protocol: Protocol) -> MDLComponent?
  weak var parent: @sil_weak MDLObject?
  var transform: MDLTransformComponent?
  var children: MDLObjectContainerComponent
  func addChild(_ child: MDLObject)
  @discardableResult
  func boundingBox(atTime time: NSTimeInterval) -> MDLAxisAlignedBoundingBox
}
@available(tvOS 9.0, *)
class MDLObjectContainer : NSObject, MDLObjectContainerComponent {
}
